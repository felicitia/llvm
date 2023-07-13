//===--- SemaFT.cpp - Semantic Analysis for FT constructs ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file implements semantic analysis for FT directives and
/// clauses.
///
//===----------------------------------------------------------------------===//

#include "TreeTransform.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTMutationListener.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclFT.h"
#include "clang/AST/FTClause.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/StmtFT.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TypeOrdering.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/FTKinds.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/ScopeInfo.h"
#include "clang/Sema/SemaInternal.h"
#include "llvm/ADT/IndexedMap.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/StringExtras.h"
//#include "llvm/Frontend/OpenMP/OMPAssume.h"
//#include "llvm/Frontend/OpenMP/OMPConstants.h"
#include "llvm/Frontend/FT/FTConstants.h"
#include <set>

using namespace clang;
using namespace llvm::ft;

namespace {
/// Default data sharing attributes, which can be applied to directive.
enum DefaultDataSharingAttributes {
  DSA_unspecified = 0,       /// Data sharing attribute not specified.
  DSA_none = 1 << 0,         /// Default data sharing attribute 'none'.
  DSA_shared = 1 << 1,       /// Default data sharing attribute 'shared'.
  DSA_firstprivate = 1 << 2, /// Default data sharing attribute 'firstprivate'.
};

/// Stack for tracking declarations used in FT directives and
/// clauses and their data-sharing attributes.
class DSAStackTy {
public:
  struct DSAVarData {
    FTDirectiveKind DKind = FTD_unknown;
    FTClauseKind CKind = FTC_unknown;
    unsigned Modifier = 0;
    const Expr *RefExpr = nullptr;
    DeclRefExpr *PrivateCopy = nullptr;
    SourceLocation ImplicitDSALoc;
    bool AppliedToPointee = false;
    DSAVarData() = default;
    DSAVarData(FTDirectiveKind DKind, FTClauseKind CKind,
               const Expr *RefExpr, DeclRefExpr *PrivateCopy,
               SourceLocation ImplicitDSALoc, unsigned Modifier,
               bool AppliedToPointee)
        : DKind(DKind), CKind(CKind), Modifier(Modifier), RefExpr(RefExpr),
          PrivateCopy(PrivateCopy), ImplicitDSALoc(ImplicitDSALoc),
          AppliedToPointee(AppliedToPointee) {}
  };
  /// Kind of the declaration used in the uses_allocators clauses.
  enum class UsesAllocatorsDeclKind {
    /// Predefined allocator
    PredefinedAllocator,
    /// User-defined allocator
    UserDefinedAllocator,
    /// The declaration that represent allocator trait
    AllocatorTrait,
  };

private:
  struct DSAInfo {
    FTClauseKind Attributes = FTC_unknown;
    unsigned Modifier = 0;
    /// Pointer to a reference expression and a flag which shows that the
    /// variable is marked as lastprivate(true) or not (false).
    llvm::PointerIntPair<const Expr *, 1, bool> RefExpr;
    DeclRefExpr *PrivateCopy = nullptr;
    /// true if the attribute is applied to the pointee, not the variable
    /// itself.
    bool AppliedToPointee = false;
  };
  using DeclSAMapTy = llvm::SmallDenseMap<const ValueDecl *, DSAInfo, 8>;
  using UsedRefMapTy = llvm::SmallDenseMap<const ValueDecl *, const Expr *, 8>;
  using LCDeclInfo = std::pair<unsigned, VarDecl *>;
  using LoopControlVariablesMapTy =
      llvm::SmallDenseMap<const ValueDecl *, LCDeclInfo, 8>;
  struct ReductionData {
    using BOKPtrType = llvm::PointerEmbeddedInt<BinaryOperatorKind, 16>;
    SourceRange ReductionRange;
    llvm::PointerUnion<const Expr *, BOKPtrType> ReductionOp;
    ReductionData() = default;
    void set(BinaryOperatorKind BO, SourceRange RR) {
      ReductionRange = RR;
      ReductionOp = BO;
    }
    void set(const Expr *RefExpr, SourceRange RR) {
      ReductionRange = RR;
      ReductionOp = RefExpr;
    }
  };
  using DeclReductionMapTy =
      llvm::SmallDenseMap<const ValueDecl *, ReductionData, 4>;
#if 0
  struct DefaultmapInfo {
    FTDefaultmapClauseModifier ImplicitBehavior =
        FTC_DEFAULTMAP_MODIFIER_unknown;
    SourceLocation SLoc;
    DefaultmapInfo() = default;
    DefaultmapInfo(FTDefaultmapClauseModifier M, SourceLocation Loc)
        : ImplicitBehavior(M), SLoc(Loc) {}
  };
#endif
  struct SharingMapTy {
    DefaultDataSharingAttributes DefaultAttr = DSA_unspecified;
    SourceLocation DefaultAttrLoc;
    FTDirectiveKind Directive = FTD_unknown;
    DeclarationNameInfo DirectiveName;
    Scope *CurScope = nullptr;
    DeclContext *Context = nullptr;
    SourceLocation ConstructLoc;
    /// First argument (Expr *) contains optional argument of the
    /// 'ordered' clause, the second one is true if the regions has 'ordered'
    /// clause, false otherwise.
    unsigned AssociatedLoops = 1;
    bool HasMutipleLoops = false;
    const Decl *PossiblyLoopCounter = nullptr;
    bool NowaitRegion = false;
    bool UntiedRegion = false;
    bool CancelRegion = false;
    bool LoopStart = false;
    bool BodyComplete = false;
    SourceLocation PrevScanLocation;
    SourceLocation PrevOrderedLocation;
    SourceLocation InnerTeamsRegionLoc;
    /// Reference to the taskgroup task_reduction reference expression.
    Expr *TaskgroupReductionRef = nullptr;
    llvm::DenseSet<QualType> MappedClassesQualTypes;
    SmallVector<Expr *, 4> InnerUsedAllocators;
    llvm::DenseSet<CanonicalDeclPtr<Decl>> ImplicitTaskFirstprivates;
    /// List of globals marked as declare target link in this target region
    /// (isFTTargetExecutionDirective(Directive) == true).
    llvm::SmallVector<DeclRefExpr *, 4> DeclareTargetLinkVarDecls;
    /// List of decls used in inclusive/exclusive clauses of the scan directive.
    llvm::DenseSet<CanonicalDeclPtr<Decl>> UsedInScanDirective;
    Expr *DeclareMapperVar = nullptr;
    SharingMapTy(FTDirectiveKind DKind, DeclarationNameInfo Name,
                 Scope *CurScope, SourceLocation Loc)
        : Directive(DKind), DirectiveName(Name), CurScope(CurScope),
          ConstructLoc(Loc) {}
    SharingMapTy() = default;
  };

  using StackTy = SmallVector<SharingMapTy, 4>;

  /// Stack of used declaration and their data-sharing attributes.
  const FunctionScopeInfo *CurrentNonCapturingFunctionScope = nullptr;
  SmallVector<std::pair<StackTy, const FunctionScopeInfo *>, 4> Stack;
  /// true, if check for DSA must be from parent directive, false, if
  /// from current directive.
  FTClauseKind ClauseKindMode = FTC_unknown;
  Sema &SemaRef;
  bool ForceCapturing = false;
  /// true if all the variables in the target executable directives must be
  /// captured by reference.
  bool ForceCaptureByReferenceInTargetExecutable = false;
  unsigned IgnoredStackElements = 0;

  /// Iterators over the stack iterate in order from innermost to outermost
  /// directive.
  using const_iterator = StackTy::const_reverse_iterator;
  const_iterator begin() const {
    return Stack.empty() ? const_iterator()
                         : Stack.back().first.rbegin() + IgnoredStackElements;
  }
  const_iterator end() const {
    return Stack.empty() ? const_iterator() : Stack.back().first.rend();
  }
  using iterator = StackTy::reverse_iterator;
  iterator begin() {
    return Stack.empty() ? iterator()
                         : Stack.back().first.rbegin() + IgnoredStackElements;
  }
  iterator end() {
    return Stack.empty() ? iterator() : Stack.back().first.rend();
  }

  // Convenience operations to get at the elements of the stack.

  bool isStackEmpty() const {
    return Stack.empty() ||
           Stack.back().second != CurrentNonCapturingFunctionScope ||
           Stack.back().first.size() <= IgnoredStackElements;
  }
  size_t getStackSize() const {
    return isStackEmpty() ? 0
                          : Stack.back().first.size() - IgnoredStackElements;
  }

  SharingMapTy *getTopOfStackOrNull() {
    size_t Size = getStackSize();
    if (Size == 0)
      return nullptr;
    return &Stack.back().first[Size - 1];
  }
  const SharingMapTy *getTopOfStackOrNull() const {
    return const_cast<DSAStackTy &>(*this).getTopOfStackOrNull();
  }
  SharingMapTy &getTopOfStack() {
    assert(!isStackEmpty() && "no current directive");
    return *getTopOfStackOrNull();
  }
  const SharingMapTy &getTopOfStack() const {
    return const_cast<DSAStackTy &>(*this).getTopOfStack();
  }

  SharingMapTy *getSecondOnStackOrNull() {
    size_t Size = getStackSize();
    if (Size <= 1)
      return nullptr;
    return &Stack.back().first[Size - 2];
  }
  const SharingMapTy *getSecondOnStackOrNull() const {
    return const_cast<DSAStackTy &>(*this).getSecondOnStackOrNull();
  }

  /// Get the stack element at a certain level (previously returned by
  /// \c getNestingLevel).
  ///
  /// Note that nesting levels count from outermost to innermost, and this is
  /// the reverse of our iteration order where new inner levels are pushed at
  /// the front of the stack.
  SharingMapTy &getStackElemAtLevel(unsigned Level) {
    assert(Level < getStackSize() && "no such stack element");
    return Stack.back().first[Level];
  }
  const SharingMapTy &getStackElemAtLevel(unsigned Level) const {
    return const_cast<DSAStackTy &>(*this).getStackElemAtLevel(Level);
  }

  DSAVarData getDSA(const_iterator &Iter, ValueDecl *D) const;

  /// Checks if the variable is a local for FT region.
  bool isFTLocal(VarDecl *D, const_iterator Iter) const;

  /// Vector of previously encountered target directives
  SmallVector<SourceLocation, 2> TargetLocations;
  SourceLocation AtomicLocation;
  /// Vector of declare variant construct traits.

public:
  explicit DSAStackTy(Sema &S) : SemaRef(S) {}

  bool isClauseParsingMode() const { return ClauseKindMode != FTC_unknown; }
  FTClauseKind getClauseParsingMode() const {
    assert(isClauseParsingMode() && "Must be in clause parsing mode.");
    return ClauseKindMode;
  }
  void setClauseParsingMode(FTClauseKind K) { ClauseKindMode = K; }

  bool isBodyComplete() const {
    const SharingMapTy *Top = getTopOfStackOrNull();
    return Top && Top->BodyComplete;
  }
  void setBodyComplete() { getTopOfStack().BodyComplete = true; }

  bool isForceVarCapturing() const { return ForceCapturing; }
  void setForceVarCapturing(bool V) { ForceCapturing = V; }

  void setForceCaptureByReferenceInTargetExecutable(bool V) {
    ForceCaptureByReferenceInTargetExecutable = V;
  }
  bool isForceCaptureByReferenceInTargetExecutable() const {
    return ForceCaptureByReferenceInTargetExecutable;
  }

  void push(FTDirectiveKind DKind, const DeclarationNameInfo &DirName,
            Scope *CurScope, SourceLocation Loc) {
    assert(!IgnoredStackElements &&
           "cannot change stack while ignoring elements");
    if (Stack.empty() ||
        Stack.back().second != CurrentNonCapturingFunctionScope)
      Stack.emplace_back(StackTy(), CurrentNonCapturingFunctionScope);
    Stack.back().first.emplace_back(DKind, DirName, CurScope, Loc);
    Stack.back().first.back().DefaultAttrLoc = Loc;
  }

  void pop() {
    assert(!IgnoredStackElements &&
           "cannot change stack while ignoring elements");
    assert(!Stack.back().first.empty() &&
           "Data-sharing attributes stack is empty!");
    Stack.back().first.pop_back();
  }

  /// RAII object to temporarily leave the scope of a directive when we want to
  /// logically operate in its parent.
  class ParentDirectiveScope {
    DSAStackTy &Self;
    bool Active;

  public:
    ParentDirectiveScope(DSAStackTy &Self, bool Activate)
        : Self(Self), Active(false) {
      if (Activate)
        enable();
    }
    ~ParentDirectiveScope() { disable(); }
    void disable() {
      if (Active) {
        --Self.IgnoredStackElements;
        Active = false;
      }
    }
    void enable() {
      if (!Active) {
        ++Self.IgnoredStackElements;
        Active = true;
      }
    }
  };

  /// Start new FT region stack in new non-capturing function.
  void pushFunction() {
    assert(!IgnoredStackElements &&
           "cannot change stack while ignoring elements");
    const FunctionScopeInfo *CurFnScope = SemaRef.getCurFunction();
    assert(!isa<CapturingScopeInfo>(CurFnScope));
    CurrentNonCapturingFunctionScope = CurFnScope;
  }
  /// Pop region stack for non-capturing function.
  void popFunction(const FunctionScopeInfo *OldFSI) {
    assert(!IgnoredStackElements &&
           "cannot change stack while ignoring elements");
    if (!Stack.empty() && Stack.back().second == OldFSI) {
      assert(Stack.back().first.empty());
      Stack.pop_back();
    }
    CurrentNonCapturingFunctionScope = nullptr;
    for (const FunctionScopeInfo *FSI : llvm::reverse(SemaRef.FunctionScopes)) {
      if (!isa<CapturingScopeInfo>(FSI)) {
        CurrentNonCapturingFunctionScope = FSI;
        break;
      }
    }
  }

  /// If 'aligned' declaration for given variable \a D was not seen yet,
  /// add it and return NULL; otherwise return previous occurrence's expression
  /// for diagnostics.
  const Expr *addUniqueAligned(const ValueDecl *D, const Expr *NewDE);
  /// If 'nontemporal' declaration for given variable \a D was not seen yet,
  /// add it and return NULL; otherwise return previous occurrence's expression
  /// for diagnostics.
  const Expr *addUniqueNontemporal(const ValueDecl *D, const Expr *NewDE);

  /// Register specified variable as loop control variable.
  void addLoopControlVariable(const ValueDecl *D, VarDecl *Capture);
  /// Check if the specified variable is a loop control variable for
  /// current region.
  /// \return The index of the loop control variable in the list of associated
  /// for-loops (from outer to inner).
  const LCDeclInfo isLoopControlVariable(const ValueDecl *D) const;
  /// Check if the specified variable is a loop control variable for
  /// parent region.
  /// \return The index of the loop control variable in the list of associated
  /// for-loops (from outer to inner).
  const LCDeclInfo isParentLoopControlVariable(const ValueDecl *D) const;
  /// Check if the specified variable is a loop control variable for
  /// current region.
  /// \return The index of the loop control variable in the list of associated
  /// for-loops (from outer to inner).
  const LCDeclInfo isLoopControlVariable(const ValueDecl *D,
                                         unsigned Level) const;
  /// Get the loop control variable for the I-th loop (or nullptr) in
  /// parent directive.
  const ValueDecl *getParentLoopControlVariable(unsigned I) const;

  /// Checks if the specified declaration was used in the inner scan directive.
  bool isUsedInScanDirective(ValueDecl *D) const {
    if (const SharingMapTy *Stack = getTopOfStackOrNull())
      return Stack->UsedInScanDirective.contains(D);
    return false;
  }

  /// Adds explicit data sharing attribute to the specified declaration.
  void addDSA(const ValueDecl *D, const Expr *E, FTClauseKind A,
              DeclRefExpr *PrivateCopy = nullptr, unsigned Modifier = 0,
              bool AppliedToPointee = false);

  /// Adds additional information for the reduction items with the reduction id
  /// represented as an operator.
  void addTaskgroupReductionData(const ValueDecl *D, SourceRange SR,
                                 BinaryOperatorKind BOK);
  /// Adds additional information for the reduction items with the reduction id
  /// represented as reduction identifier.
  void addTaskgroupReductionData(const ValueDecl *D, SourceRange SR,
                                 const Expr *ReductionRef);

  /// Returns data sharing attributes from top of the stack for the
  /// specified declaration.
  const DSAVarData getTopDSA(ValueDecl *D, bool FromParent);
  /// Returns data-sharing attributes for the specified declaration.
  const DSAVarData getImplicitDSA(ValueDecl *D, bool FromParent) const;
  /// Returns data-sharing attributes for the specified declaration.
  const DSAVarData getImplicitDSA(ValueDecl *D, unsigned Level) const;
  /// Checks if the specified variables has data-sharing attributes which
  /// match specified \a CPred predicate in any directive which matches \a DPred
  /// predicate.
  const DSAVarData
  hasDSA(ValueDecl *D,
         const llvm::function_ref<bool(FTClauseKind, bool)> CPred,
         const llvm::function_ref<bool(FTDirectiveKind)> DPred,
         bool FromParent) const;
  /// Checks if the specified variables has data-sharing attributes which
  /// match specified \a CPred predicate in any innermost directive which
  /// matches \a DPred predicate.
  const DSAVarData
  hasInnermostDSA(ValueDecl *D,
                  const llvm::function_ref<bool(FTClauseKind, bool)> CPred,
                  const llvm::function_ref<bool(FTDirectiveKind)> DPred,
                  bool FromParent) const;
  /// Checks if the specified variables has explicit data-sharing
  /// attributes which match specified \a CPred predicate at the specified
  /// FT region.
  bool
  hasExplicitDSA(const ValueDecl *D,
                 const llvm::function_ref<bool(FTClauseKind, bool)> CPred,
                 unsigned Level, bool NotLastprivate = false) const;

  /// Returns true if the directive at level \Level matches in the
  /// specified \a DPred predicate.
  bool hasExplicitDirective(
      const llvm::function_ref<bool(FTDirectiveKind)> DPred,
      unsigned Level) const;

  /// Finds a directive which matches specified \a DPred predicate.
  bool hasDirective(
      const llvm::function_ref<bool(
          FTDirectiveKind, const DeclarationNameInfo &, SourceLocation)>
          DPred,
      bool FromParent) const;

  /// Returns currently analyzed directive.
  FTDirectiveKind getCurrentDirective() const {
    const SharingMapTy *Top = getTopOfStackOrNull();
    return Top ? Top->Directive : FTD_unknown;
  }
  /// Returns directive kind at specified level.
  FTDirectiveKind getDirective(unsigned Level) const {
    assert(!isStackEmpty() && "No directive at specified level.");
    return getStackElemAtLevel(Level).Directive;
  }
  /// Returns the capture region at the specified level.
  FTDirectiveKind getCaptureRegion(unsigned Level,
                                       unsigned FTCaptureLevel) const {
    SmallVector<FTDirectiveKind, 4> CaptureRegions;
    getFTCaptureRegions(CaptureRegions, getDirective(Level));
    return CaptureRegions[FTCaptureLevel];
  }
  /// Returns parent directive.
  FTDirectiveKind getParentDirective() const {
    const SharingMapTy *Parent = getSecondOnStackOrNull();
    return Parent ? Parent->Directive : FTD_unknown;
  }

  /// Add location of previously encountered target to internal vector
  void addTargetDirLocation(SourceLocation LocStart) {
    TargetLocations.push_back(LocStart);
  }

  // Return previously encountered target region locations.
  ArrayRef<SourceLocation> getEncounteredTargetLocs() const {
    return TargetLocations;
  }

  /// Set default data sharing attribute to none.
  void setDefaultDSANone(SourceLocation Loc) {
    getTopOfStack().DefaultAttr = DSA_none;
    getTopOfStack().DefaultAttrLoc = Loc;
  }

  DefaultDataSharingAttributes getDefaultDSA(unsigned Level) const {
    return getStackSize() <= Level ? DSA_unspecified
                                   : getStackElemAtLevel(Level).DefaultAttr;
  }
  DefaultDataSharingAttributes getDefaultDSA() const {
    return isStackEmpty() ? DSA_unspecified : getTopOfStack().DefaultAttr;
  }
  SourceLocation getDefaultDSALocation() const {
    return isStackEmpty() ? SourceLocation() : getTopOfStack().DefaultAttrLoc;
  }

  /// Set collapse value for the region.
  void setAssociatedLoops(unsigned Val) {
    getTopOfStack().AssociatedLoops = Val;
    if (Val > 1)
      getTopOfStack().HasMutipleLoops = true;
  }
  /// Return collapse value for region.
  unsigned getAssociatedLoops() const {
    const SharingMapTy *Top = getTopOfStackOrNull();
    return Top ? Top->AssociatedLoops : 0;
  }
  /// Returns true if the construct is associated with multiple loops.
  bool hasMutipleLoops() const {
    const SharingMapTy *Top = getTopOfStackOrNull();
    return Top ? Top->HasMutipleLoops : false;
  }


  Scope *getCurScope() const {
    const SharingMapTy *Top = getTopOfStackOrNull();
    return Top ? Top->CurScope : nullptr;
  }
  void setContext(DeclContext *DC) { getTopOfStack().Context = DC; }
  SourceLocation getConstructLoc() const {
    const SharingMapTy *Top = getTopOfStackOrNull();
    return Top ? Top->ConstructLoc : SourceLocation();
  }

  void addDeclareMapperVarRef(Expr *Ref) {
    SharingMapTy &StackElem = getTopOfStack();
    StackElem.DeclareMapperVar = Ref;
  }
  const Expr *getDeclareMapperVarRef() const {
    const SharingMapTy *Top = getTopOfStackOrNull();
    return Top ? Top->DeclareMapperVar : nullptr;
  }
};

} // namespace
void Sema::StartFTDSABlock(FTDirectiveKind DKind,
                               const DeclarationNameInfo &DirName,
                               Scope *CurScope, SourceLocation Loc) {
//  DSAStack->push(DKind, DirName, CurScope, Loc);
  PushExpressionEvaluationContext(
      ExpressionEvaluationContext::PotentiallyEvaluated);
}

void Sema::EndFTDSABlock(Stmt *CurDirective) {
  // FT [2.14.3.5, Restrictions, C/C++, p.1]
  //  A variable of class type (or array thereof) that appears in a lastprivate
  //  clause requires an accessible, unambiguous default constructor for the
  //  class type, unless the list item is also specified in a firstprivate
  //  clause.

//  DSAStack->pop();
  DiscardCleanupsInEvaluationContext();
  PopExpressionEvaluationContext();
}
void Sema::StartFTClause(FTClauseKind K) {
//  DSAStack->setClauseParsingMode(K);
}

void Sema::EndFTClause() {
//  DSAStack->setClauseParsingMode(/*K=*/FTC_unknown);
  CleanupVarDeclMarking();
}

StmtResult Sema::ActOnFTExecutableDirective(
    FTDirectiveKind Kind, const DeclarationNameInfo &DirName,
    ArrayRef<FTClause *> Clauses,
    Stmt *AStmt, SourceLocation StartLoc, SourceLocation EndLoc) {
  StmtResult Res = StmtError();
//  FTBindClauseKind BindKind = FTC_BIND_unknown;
  llvm::SmallVector<FTClause *, 8> ClausesWithImplicit;
  bool ErrorFound = false;
  ClausesWithImplicit.append(Clauses.begin(), Clauses.end());

  llvm::SmallVector<FTDirectiveKind, 4> AllowedNameModifiers;
  switch (Kind) {
  case FTD_vote:
    assert(AStmt == nullptr &&
           "No associated statement allowed for 'ft vote' directive");
    Res = ActOnFTVoteDirective(ClausesWithImplicit, StartLoc, EndLoc);
    break;
  case FTD_nmr:
    Res = ActOnFTNmrDirective(ClausesWithImplicit, AStmt, StartLoc,
                                       EndLoc);
    break;
  default:
    llvm_unreachable("Unknown FT directive");
  }

  ErrorFound = Res.isInvalid() || ErrorFound;

  if (ErrorFound)
    return StmtError();

  return Res;
}

StmtResult Sema::ActOnFTVoteDirective(ArrayRef<FTClause *> Clauses,
                                           SourceLocation StartLoc,
                                           SourceLocation EndLoc) {
#if 0
  FTVoteClause *FC = nullptr;
  FTClause *OrderClause = nullptr;
  for (FTClause *C : Clauses) {
    if (C->getClauseKind() == FTC_vote)
      FC = cast<FTVoteClause>(C);
    else
      OrderClause = C;
  }
#endif
  return FTVoteDirective::Create(Context, StartLoc, EndLoc, Clauses);
}
#if 1
FTClause *Sema::ActOnFTVoteClause(ArrayRef<Expr *> VarList, 
					ArrayRef<Expr *> SizeList,
					ArrayRef<Expr *> PtrList,
                                        SourceLocation StartLoc,
                                        SourceLocation LParenLoc,
                                        SourceLocation EndLoc) {
  if (VarList.empty())
    return nullptr;

  return FTVoteClause::Create(Context, StartLoc, LParenLoc, EndLoc, VarList, SizeList, PtrList);
}
#endif
StmtResult Sema::ActOnFTNmrDirective(ArrayRef<FTClause *> Clauses,
                                              Stmt *AStmt,
                                              SourceLocation StartLoc,
                                              SourceLocation EndLoc) {

  if (!AStmt)
    return StmtError();
  return FTNmrDirective::Create(Context, StartLoc, EndLoc, Clauses, AStmt);
}
  
FTClause *Sema::ActOnFTVarSizeListClause(FTClauseKind Kind,
					 ArrayRef<Expr *> VarList,
					 ArrayRef<Expr *> SizeList,
					 ArrayRef<Expr *> PtrList,
                                         SourceLocation StartLoc,
                                         SourceLocation LParenLoc,
                                         SourceLocation EndLoc) {
  SmallVector<Expr *, 4> Vars;
  SmallVector<Expr *, 4> SizeL;
  SmallVector<Expr *, 4> Ptr;
  // DK TODO: recode with array index
  for (int i = 0; i < (int)VarList.size(); i++) {
#if 1
    Vars.push_back(VarList[i]);
    SizeL.push_back(SizeList[i]);
    Ptr.push_back(PtrList[i]);
#else
    SourceLocation ELoc;
    SourceRange ERange;
    Expr *SimpleRefExpr = VarList[i];
    auto Res = getPrivateItem(*this, SimpleRefExpr, ELoc, ERange);
    if (Res.second) {
      // It will be analyzed later.
      Vars.push_back(VarList[i]);
      SizeL.push_back(SizeList[i]);
      Ptr.push_back(PtrList[i]);
    }
    ValueDecl *D = Res.first;
    if (!D)
      continue;
    auto *VD = dyn_cast<VarDecl>(D);

    DeclRefExpr *Ref = nullptr;
    if (!VD /* && isFTCapturedDecl(D) */ && !CurContext->isDependentContext())
      Ref = buildCapture(*this, D, SimpleRefExpr, /*WithInit=*/true);
    Vars.push_back((VD || !Ref || CurContext->isDependentContext())
                       ? VarList[i]->IgnoreParens()
                       : Ref);
#endif
  }

  if (Vars.empty())
    return nullptr;

  switch(Kind){
    case FTC_vote:
      return FTVoteClause::Create(Context, StartLoc, LParenLoc, EndLoc, Vars, SizeL, Ptr);
    case FTC_rhs:
      return FTRhsClause::Create(Context, StartLoc, LParenLoc, EndLoc, Vars, SizeL, Ptr);
    case FTC_lhs:
      return FTLhsClause::Create(Context, StartLoc, LParenLoc, EndLoc, Vars, SizeL, Ptr);
    case FTC_novote:
      return FTNovoteClause::Create(Context, StartLoc, LParenLoc, EndLoc, Vars, SizeL, Ptr);
    case FTC_norhs:
      return FTNorhsClause::Create(Context, StartLoc, LParenLoc, EndLoc, Vars, SizeL, Ptr);
    case FTC_nolhs:
      return FTNolhsClause::Create(Context, StartLoc, LParenLoc, EndLoc, Vars, SizeL, Ptr);
    case FTC_auto:
      return FTAutoClause::Create(Context, StartLoc, LParenLoc, EndLoc, Vars, SizeL, Ptr);
    default:
      return nullptr;
  }
}
// endif
// #endif

