//===--- CGStmtNMR.cpp - Emit LLVM Code from Statements ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This contains code to emit FT nodes as LLVM code.
//
//===----------------------------------------------------------------------===//
#include "CGCleanup.h"
#include "CGFTRuntime.h"
#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "TargetInfo.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/FTClause.h"	// TODO
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtFT.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Basic/PrettyStackTrace.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Metadata.h"
#include "llvm/Support/AtomicOrdering.h"

#include "clang/AST/RecursiveASTVisitor.h"

#include "clang/ASTMatchers/ASTMatchFinder.h"

//#include "CGFTRuntime.h"

using namespace clang;
using namespace CodeGen;
using namespace llvm::omp;
static bool isSameExpr(const Expr * E1, const Expr * E2) {
    const DeclRefExpr * DR = cast<DeclRefExpr>(E1);
    const VarDecl *VD = dyn_cast<VarDecl>(DR->getDecl());
    const DeclRefExpr * DR2 = cast<DeclRefExpr>(E2);
    const VarDecl *VD2 = dyn_cast<VarDecl>(DR2->getDecl());
    if (VD->getQualifiedNameAsString() == VD2->getQualifiedNameAsString()
          && VD->getDeclContext() == VD2->getDeclContext()) return true;
    else return false;
}

namespace {
class PointerChecker : public clang::RecursiveASTVisitor<PointerChecker> {
public:
  bool hasPointer(const QualType Ty) {
    if (const auto *RT = Ty->getAs<RecordType>()) {
      hasPointer(RT->getDecl());
    }
    return true;
  }
  bool hasPointer(const clang::RecordDecl *recordDecl) {
    return TraverseRecordDecl(const_cast<clang::RecordDecl *>(recordDecl));
  }

  bool VisitPointerType(const clang::PointerType *pointerType) {
    hasPointer_ = true;
    return false;
  }

private:
  bool TraverseRecordDecl(clang::RecordDecl *recordDecl) {
    for (auto field : recordDecl->fields()) {
      if (field->getType()->isPointerType()) {
        hasPointer_ = true;
        return false;
      }
      const auto * RT = field->getType()->getAs<RecordType>();
      clang::RecordDecl * RD = RT->getDecl();
      if (RD) {
        TraverseRecordDecl(RD);
      } 
    }
    return true;
  }

  bool hasPointer_ = false;

public:
  bool hasPointer() const { return hasPointer_; }
};
}

static void removeVars(CodeGenFunction &CGF, SmallVector<const Expr *, 4> &VarSize, SmallVector<const Expr*, 4> &DeleteVars) {
  if (VarSize.size() == 0 || DeleteVars.size() == 0) return;
  SmallVector<int, 4> indices;
  for (int i=0; i < (int)DeleteVars.size(); i+=3) {
    // pointer depth
    int const1, const2;
    if (DeleteVars[i+2] == nullptr) const1 = 0;
    else { 
      auto const_int1 = cast<llvm::ConstantInt>(CGF.EmitScalarExpr(DeleteVars[i+2]));
      const1 = const_int1->getSExtValue();
    }
    for (int j=0; j < (int)VarSize.size(); j+=3) {
      // pointer depth
      if (VarSize[i+2] == nullptr) const2 = 0;
      else {
	auto const_int2 = cast<llvm::ConstantInt>(CGF.EmitScalarExpr(VarSize[i+2]));
        const2 = const_int2->getSExtValue();
      }
      if (isSameExpr(DeleteVars[i], VarSize[j]) && const1 == const2)
      // TODO: check if their othre fiels (size, ptr) are also the same
        indices.push_back(j);
    }
  }
  for (int i = indices.size() - 1; i >= 0; i--) {
    VarSize.erase(VarSize.begin() + indices[i] + 2);
    VarSize.erase(VarSize.begin() + indices[i] + 1);
    VarSize.erase(VarSize.begin() + indices[i]);
  }
}

static int isVarIncluded(const Expr * E,  SmallVector<const Expr *, 4> &VarList) {
  auto *SaveRef = cast<DeclRefExpr>(E->IgnoreImpCasts());
  if (SaveRef == nullptr) return -1;
  const VarDecl *VD = dyn_cast<VarDecl>(SaveRef->getDecl());
  if (VD == nullptr) return -1;
  for (int i=0; i < (int)VarList.size(); i+=3) {
    const DeclRefExpr * DR = cast<DeclRefExpr>(VarList[i]);
    const VarDecl *VD2 = dyn_cast<VarDecl>(DR->getDecl());
    if (VD->getQualifiedNameAsString() == VD2->getQualifiedNameAsString()
        && VD->getDeclContext() == VD2->getDeclContext()) 
        return i;
  }
  return -1;
}

static const Expr * visitExpr(const DeclRefExpr *E, SmallVector<const Expr *, 4> &VarSize,
		std::vector<int> &VarsSizesIndex, bool lookforLHS, bool canbeLHS) {
  const Expr * FoundExpr = nullptr;
  if (!E || VarSize.size() == 0 || (lookforLHS && !canbeLHS) || (!lookforLHS && canbeLHS)) return FoundExpr;
  int i = isVarIncluded(E, VarSize);
  if (i < 0) return nullptr;
  int j;
  FoundExpr = VarSize[i];
  for (j = 0; j < (int)VarsSizesIndex.size(); j++) {
    if (VarsSizesIndex[j] == i) // already included
      break;
  }
  if (j >= (int)VarsSizesIndex.size()) 
    VarsSizesIndex.push_back(i);
  return FoundExpr;
}

static const Expr * searchVoteVar(const Stmt *S, SmallVector<const Expr *, 4> &VarSize,
		std::vector<int> &VarsNameIndex, bool lookforLHS, bool canbeLHS) {

  const Expr * FoundVar = nullptr;
  bool canbeLHS2 = canbeLHS;

  if (!S || VarSize.size() == 0) return nullptr;

  switch (S->getStmtClass()) {
    case Stmt::DeclRefExprClass: {
      auto *SaveRef = cast<DeclRefExpr>((cast<DeclRefExpr>(S))->IgnoreImpCasts());
      if (SaveRef == nullptr || (lookforLHS && !canbeLHS)) return nullptr;
      const VarDecl *VD = dyn_cast<VarDecl>(SaveRef->getDecl());
      if (VD == nullptr) return FoundVar;
      for (int i=0; i < (int)VarSize.size(); i+=3) {
        const DeclRefExpr * DR = cast<DeclRefExpr>(VarSize[i]);
        const VarDecl *VD2 = dyn_cast<VarDecl>(DR->getDecl());
        if (VD->getQualifiedNameAsString() == VD2->getQualifiedNameAsString()
            && VD->getDeclContext() == VD2->getDeclContext()) {
          for (int j = 0; j < (int)VarsNameIndex.size(); j++) {
            if (VarsNameIndex[j] == i) // already included
              return VarSize[i];
          }
          VarsNameIndex.push_back(i);
          return VarSize[i];
        }
      }
      return nullptr;
    }
    case Stmt::UnaryOperatorClass: {
      const UnaryOperator * UO = cast<UnaryOperator>(S);
      if (UO->isPrefix() || UO->isPostfix()) {
        canbeLHS2 = (lookforLHS ? true : false );
      } 
      if (UO->getOpcode() == UO_AddrOf || UO->getOpcode() == UO_Deref) {
      }
      break;
    }
    case Stmt::BinaryOperatorClass: {
      const BinaryOperator * BO = cast<BinaryOperator>(S);
      if (BO->getOpcode() == BO_Assign) {
        FoundVar = searchVoteVar(cast<Stmt>(BO->getLHS()), VarSize, VarsNameIndex, lookforLHS, true);
        if (FoundVar != nullptr) return FoundVar;
        canbeLHS2 = false;
      }
      break;
    }
    case Stmt::AtomicExprClass: {	// For atomic, assume all variables can be LHS
      canbeLHS2 = (lookforLHS ? true : canbeLHS);	
      break;
    }
    default:
      break;
  }
  for (const Stmt *subStmt : S->children()) {
    if (!subStmt) continue;
    FoundVar = searchVoteVar(subStmt, VarSize, VarsNameIndex, lookforLHS, canbeLHS2);
    if (FoundVar) return FoundVar;
  }
  return nullptr;
}

static const Expr * visitStmt(const Stmt *S, SmallVector<const Expr *, 4> &VarSize,
		std::vector<int> &VarsNameIndex, bool lookforLHS, bool canbeLHS) {
  const Expr * FoundVar = nullptr;
  if (!S || VarSize.size() == 0) return FoundVar;
  switch (S->getStmtClass()) {
  case Stmt::ImplicitCastExprClass: {
    const ImplicitCastExpr * E = cast<ImplicitCastExpr>(S);
    return visitStmt(cast<Stmt>(E->getSubExpr()), VarSize, VarsNameIndex, lookforLHS, canbeLHS);
    }
  case Stmt::CompoundStmtClass: 
    for (auto *InnerStmt : cast<CompoundStmt>(S)->body()) {
      if (!InnerStmt) continue;
       std::vector<int> _VarsNameIndex;
       visitStmt(InnerStmt, VarSize, _VarsNameIndex, lookforLHS, false);
    }
    return nullptr;
  case Stmt::UnaryOperatorClass: {
    const UnaryOperator * UO = cast<UnaryOperator>(S);
    if (UO->isPrefix() || UO->isPostfix() || UO->getOpcode() == UO_AddrOf || UO->getOpcode() == UO_Deref) {
      if (lookforLHS) return visitStmt(cast<Stmt>(UO->getSubExpr()), VarSize, VarsNameIndex, lookforLHS, true);
      // Should we include it as Rvalue, too.
      if (!lookforLHS) return visitStmt(cast<Stmt>(UO->getSubExpr()), VarSize, VarsNameIndex, lookforLHS, false);
    }
    else { 
      if (!lookforLHS)
        return visitStmt(cast<Stmt>(UO->getSubExpr()), VarSize, VarsNameIndex, lookforLHS, false);
    }
    return FoundVar;
    }
  case Stmt::ArraySubscriptExprClass: 
    {
    const ArraySubscriptExpr * ARS = cast<ArraySubscriptExpr>(S);
    FoundVar = visitStmt(cast<Stmt>(ARS->getLHS()), VarSize, VarsNameIndex, lookforLHS, canbeLHS);
    const Expr * FoundVar2 = visitStmt(cast<Stmt>(ARS->getRHS()), VarSize, VarsNameIndex, lookforLHS, false);
    if (FoundVar == nullptr) FoundVar = FoundVar2;
    return FoundVar;
    }
  case Stmt::MemberExprClass: 
    {
    const MemberExpr * MES = cast<MemberExpr>(S);
    FoundVar = visitStmt(cast<Stmt>(MES->getBase()), VarSize, VarsNameIndex, lookforLHS, canbeLHS);
/*    const Expr * FoundVar2 = visitStmt(cast<Stmt>(MES->getRHS()), VarSize, VarsNameIndex, lookforLHS, false);
    if (FoundVar == nullptr) FoundVar = FoundVar2; */
    return FoundVar;
    }
  case Stmt::BinaryOperatorClass: {
    const BinaryOperator * BO = cast<BinaryOperator>(S);
    if (BO->getOpcode() == BO_Assign) {
        FoundVar = visitStmt(cast<Stmt>(BO->getLHS()), VarSize, VarsNameIndex, lookforLHS, true);
        const Expr * FoundVar2 = visitStmt(cast<Stmt>(BO->getRHS()), VarSize, VarsNameIndex, lookforLHS, false);
        if (FoundVar == nullptr) FoundVar = FoundVar2;
    } else {
        FoundVar = visitStmt(cast<Stmt>(BO->getLHS()), VarSize, VarsNameIndex, lookforLHS, false);
        const Expr * FoundVar2 = visitStmt(cast<Stmt>(BO->getRHS()), VarSize, VarsNameIndex, lookforLHS, false);
        if (FoundVar == nullptr) FoundVar = FoundVar2;
    }
    return FoundVar;
    }
  case Stmt::CompoundAssignOperatorClass: {
    const CompoundAssignOperator * CO = cast<CompoundAssignOperator>(S);
    FoundVar = visitStmt(cast<Stmt>(CO->getLHS()), VarSize, VarsNameIndex, lookforLHS, true);
    const Expr * FoundVar2 = visitStmt(cast<Stmt>(CO->getRHS()), VarSize, VarsNameIndex, lookforLHS, false);
    if (FoundVar == nullptr) FoundVar = FoundVar2;
    return FoundVar;
    }
  case Stmt::DeclStmtClass: 
    return nullptr;
  case Stmt::DeclRefExprClass:
    return visitExpr(cast<DeclRefExpr>(S) , VarSize, VarsNameIndex, lookforLHS, canbeLHS);
  case Stmt::NullStmtClass:
    return nullptr;
  case Stmt::CaseStmtClass:
    {
    if (lookforLHS) return nullptr;
    const CaseStmt * CS = cast<CaseStmt>(S);
    FoundVar = visitStmt(cast<Stmt>(CS->getLHS()), VarSize, VarsNameIndex, lookforLHS, false);
    const Expr * FoundVar2 = visitStmt(cast<Stmt>(CS->getRHS()), VarSize, VarsNameIndex, lookforLHS, false);
    if (FoundVar == nullptr) FoundVar = FoundVar2;
    return FoundVar;
    }
  default: 
    if (lookforLHS) return nullptr;
    for (const Stmt *subStmt : S->children()) {
      if (!subStmt)
        continue;
      const Expr * FoundVar2 = visitStmt(subStmt, VarSize, VarsNameIndex, lookforLHS, false);
      if (FoundVar == nullptr) FoundVar = FoundVar2;
    }
    return FoundVar;
  }
}

static uint64_t _getTypeSize(CodeGenFunction &CGF, QualType dataType) {
    uint64_t sizeInBytes = 0;

    if (const ComplexType *CTy = dataType->getAs<ComplexType>())
      dataType = CTy->getElementType();
    if (const AtomicType *ATy = dataType->getAs<AtomicType>())
      dataType = ATy->getValueType();
    if (!dataType->isAggregateType() && !dataType->isConstantMatrixType() && !dataType->isVectorType() && !dataType->getAs<BuiltinType>() && !dataType->hasPointerRepresentation() &&
        !dataType->isEnumeralType() && !dataType->isBlockPointerType()) {
      // aggregate: if there is a pointer in it, do not vote
      PointerChecker checker;
      if (checker.hasPointer(dataType)) return 0;
      const clang::Type * Type = dataType.getTypePtr();
      sizeInBytes = CGF.CGM.getContext().getTypeSizeInChars(Type).getQuantity();
    } else {
      // basic type 
      sizeInBytes = CGF.CGM.getContext().getTypeSize(dataType)/8;
    }
    return sizeInBytes;
}

static void emitVoteStmt(CodeGenFunction &CGF, SmallVector<const Expr *, 4> &VarsSizes, SourceLocation Loc) {
   if (VarsSizes.size() == 0) return;
   CGF.VoteNow = true;
   uint64_t sizeInBytes = 0;
   for (int i = 0; i < (int)VarsSizes.size(); i+=3) {
     llvm::Value * VarPtr;
     if (VarsSizes[i]->getType()->isPointerType()) { // only (Basetype *) is allowed.
       if (VarsSizes[i+1] == nullptr) return; // Not allowed! 
       LValue LV = CGF.EmitCheckedLValue(VarsSizes[i], CodeGenFunction::TCK_Load);
//       llvm::Type * Type = CGF.ConvertType(LV.getType());
//       sizeInBytes = CGF.CGM.getDataLayout().getTypeAllocSize(Type->getNonOpaquePointerElementType());
       RValue RV = CGF.EmitLoadOfLValue(LV, Loc);
       VarPtr = RV.getScalarVal();
       sizeInBytes = CGF.CGM.getDataLayout().getTypeSizeInBits(VarPtr->getType())/8; // FIXIT
//       sizeInBytes = CGF.CGM.getDataLayout().getTypeAllocSize(VarPtr->getType()->getNonOpaquePointerElementType());
     } else {
       LValue LV = CGF.EmitLValue(VarsSizes[i]);
       VarPtr = LV.getPointer(CGF);
     }

     if (VarsSizes[i+1] == nullptr) {
       if (sizeInBytes == 0) 
         sizeInBytes = _getTypeSize(CGF, VarsSizes[i]->getType());
     } else {
         auto const_int1 = cast<llvm::ConstantInt>(CGF.EmitScalarExpr(VarsSizes[i+1]));
         sizeInBytes = const_int1->getSExtValue();
     }
     CGF.VoteVar = VarsSizes[i];
     CGF.VoteNow = true;
     CGF.VoteLoc = Loc;
     CGF.EmitVoteCall(VarPtr, sizeInBytes, 0x8, false);
  }
}

void CodeGenFunction::EmitFTVoteDirective(const FTVoteDirective &S) {
	// no associated statement
  if (const auto *FtvarClause = S.getSingleClause<FTVoteClause>()) {
    SmallVector<const Expr *, 4> VarsSizes;
    VarsSizes.append(FtvarClause->varlist_begin(), FtvarClause->varlist_end());
    emitVoteStmt(*this, VarsSizes, S.getBeginLoc());
  }
}

const Expr * CodeGenFunction::EmitVarVote(const Stmt* S, SmallVector<const Expr *, 4> &VarSize, bool lookforLHS, bool generate_vote) {
  std::vector<int> VarsNameIndex;
  SmallVector<const Expr *, 4> TVarsSizes;
  const Expr * FoundVar = nullptr;
  if (VarSize.size() == 0) return FoundVar;

  FoundVar = visitStmt(S, VarSize, VarsNameIndex, lookforLHS, lookforLHS);
  const Expr * FoundVar2 = searchVoteVar(S, VarSize, VarsNameIndex, lookforLHS, lookforLHS);
  if (FoundVar != FoundVar2)
    printf("Different!\n");

  if (VarsNameIndex.size() == 0) return nullptr;

  for (int i=0; i < (int)VarsNameIndex.size(); i++) {
    TVarsSizes.push_back(VarSize[VarsNameIndex[i]]);
    TVarsSizes.push_back(VarSize[VarsNameIndex[i]+1]);
    TVarsSizes.push_back(VarSize[VarsNameIndex[i]+2]);
  }

  if (!generate_vote) return FoundVar2;
  else  emitVoteStmt(*this, TVarsSizes, S->getBeginLoc());
  return FoundVar2;
}

void CodeGenFunction::EmitFTNmrDirective(const FTNmrDirective &S) {

  SmallVector<const Expr *, 4> SaveLVarSize;
  SmallVector<const Expr *, 4> SaveRVarSize;
  SmallVector<const Expr *, 4> SaveAutoSize;
  SmallVector<const Expr *, 4> NovarSize;
  SmallVector<const Expr *, 4> NorvarSize;
  SmallVector<const Expr *, 4> NovoteSize;

  const auto *LvarClause = S.getSingleClause<FTLhsClause>();
  const auto *RvarClause = S.getSingleClause<FTRhsClause>();
  const auto *NovarClause = S.getSingleClause<FTNolhsClause>();
  const auto *NorvarClause = S.getSingleClause<FTNorhsClause>();
  const auto *NovoteClause = S.getSingleClause<FTNovoteClause>();
  const auto *AutoClause = S.getSingleClause<FTAutoClause>();
  std::vector<int> LvarsIndex, RvarsIndex;

  SaveLVarSize = LVarSize;
  SaveRVarSize = RVarSize;
  SaveAutoSize = AutoSize;

  // inherit outer NMR region's var, rvar and add local ones
  if (LvarClause)  // lvar
    LVarSize.append(LvarClause->varlist_begin(), LvarClause->varlist_end());
  if (RvarClause)  // rvar
    RVarSize.append(RvarClause->varlist_begin(), RvarClause->varlist_end());
  if (NovarClause)
    NovarSize.append(NovarClause->varlist_begin(), NovarClause->varlist_end());
  if (NorvarClause)
    NorvarSize.append(NorvarClause->varlist_begin(), NorvarClause->varlist_end());
  if (NovoteClause)
    NovoteSize.append(NovoteClause->varlist_begin(), NovoteClause->varlist_end());
  if (AutoClause)
    AutoSize.append(AutoClause->varlist_begin(), AutoClause->varlist_end());
  // remove entries in (No) clauses
  removeVars(*this, LVarSize, NovarSize);
  removeVars(*this, LVarSize, NovoteSize);
  removeVars(*this, RVarSize, NorvarSize);
  removeVars(*this, RVarSize, NovoteSize);
  // remove entries in (No) votes from Auto: TODO?: noauto?
  removeVars(*this, AutoSize, NovoteSize);
  LexicalScope Scope(*this, S.getSourceRange());
  EmitStopPoint(&S);
  const auto *CS = cast_or_null<Stmt>(S.getAssociatedStmt());
  EmitStmt(CS);

  LVarSize = SaveLVarSize;
  RVarSize = SaveRVarSize;
  AutoSize = SaveAutoSize;
}


// mode: 0 (LHS), 1 (RHS), 9 (both), mode | 0x100 (ignore pointer)
void CodeGenFunction::CheckVote(const Expr *E, int mode) {
//  bool _VoteNow = VoteNow;
//  const Expr * _VoteVar = VoteVar;
  VoteVar = nullptr;
  VoteNow = false;
  if ((mode & 0x100) == 0 && E->getType()->isPointerType() /* && mode == 0 */) return;
  mode = mode & 0xff;
  VoteLoc = E->getExprLoc();
  VoteExp = E;
  VoteVar = EmitVarVote(E, AutoSize, false, false);
  if (VoteVar != nullptr) { VoteNow = true; return; }
  VoteNow = false;
  VoteVar = nullptr;
  if (mode == 0 || mode == 9)
    VoteVar = EmitVarVote(E, LVarSize, true, false);
  if (VoteVar != nullptr) { VoteNow = true; return; }
  VoteNow = false;
  VoteVar = nullptr;
  if (mode == 1 || mode == 9)
    VoteVar = EmitVarVote(E, RVarSize, false, false);
  if (VoteVar != nullptr) { VoteNow = true; return; }
}

void CodeGenFunction::EmitVote(LValue LV, int mode , bool keep_status) {
//    bool _VoteNow = VoteNow;
//    const Expr * _VoteVar = VoteVar;
    if (VoteVar == nullptr || VoteNow == false) return; 
    Address addr = LV.getAddress(*this);
    QualType dataType = LV.getType();
//    llvm::Type *Type = ConvertType(dataType);
    if (dataType->isPointerType() && LV.getPointer(*this)) return;
    if (!dataType->isPointerType() && !(LV.getPointer(*this))) return;
    EmitVote(addr, dataType, mode, keep_status);
}

void CodeGenFunction::EmitVote(Address addrR, QualType dataTypeR, Address addrI, QualType dataTypeI, int mode , bool keep_status) {
    if (VoteVar == nullptr || VoteNow == false) return; 
    bool _VoteNow = VoteNow;
    const Expr * _VoteVar = VoteVar;
    EmitVote(addrR, dataTypeR, mode, true);
    VoteNow = _VoteNow;
    VoteVar = _VoteVar;
    EmitVote(addrI, dataTypeI, mode, keep_status);
}

void CodeGenFunction::EmitVote(Address addr, QualType dataType, int mode, bool keep_status) {
    if (VoteVar == nullptr || VoteNow == false) return; 
    EmitVoteCall(addr.getPointer(), _getTypeSize(*this, dataType), mode, keep_status);
}

void CodeGenFunction::EmitVote(int mode, bool keep_status) {
// TODO:    need to generage TCK_load
   if (VoteVar == nullptr || VoteNow == false) return; 
   LValue LV = EmitCheckedLValue(VoteVar, CodeGenFunction::TCK_Load);
   llvm::Value * VarPtr = LV.getPointer(*this);
   uint64_t sizeInBytes = _getTypeSize(*this, VoteVar->getType());
   EmitVoteCall(VarPtr, sizeInBytes, mode, keep_status);
}

void CodeGenFunction::EmitVote(const Expr * E, LValue LHS) {
  // DK: NEW vote after this (LHS)
  CheckVote(E, 0);
  if (VoteVar == nullptr || VoteNow == false) return; 
  EmitVoteCall(LHS.getPointer(*this), _getTypeSize(*this, LHS.getType()), 0, false);
}

// whichSide: 0 (LHS), 1 (RHS), 0x2 (atomic), 0x3 (auto), 8 (vote now)
void CodeGenFunction::EmitVoteCall(llvm::Value * AddrPtr, QualType dataType, int whichSide, bool keep_status) {
    if (VoteVar == nullptr || VoteNow == false) return; 
    uint64_t sizeOfType = _getTypeSize(*this, dataType);
    if (whichSide == 1) // store
      sizeOfType = _getTypeSize(*this, VoteExp->getType());
    EmitVoteCall(AddrPtr, sizeOfType, whichSide, keep_status);
}

void CodeGenFunction::EmitVoteCall(llvm::Value * AddrPtr, uint64_t sizeOfType, int whichSide, bool keep_status) {

    if (VoteVar == nullptr || VoteNow == false) return;
    if (sizeOfType == 0) {
      printf("Warning: Size of type is 0\n"); 
      VoteNow = false;
      VoteVar = nullptr;
      return;
    }
    bool isDebug = false;
    llvm::Value *TSize = llvm::ConstantInt::get(Int32Ty, sizeOfType);
    const DeclRefExpr * DR = cast<DeclRefExpr>(VoteVar);
    const VarDecl *VD = dyn_cast<VarDecl>(DR->getDecl());
    llvm::Constant* constStr = llvm::ConstantDataArray::getString(getLLVMContext(), VD->getQualifiedNameAsString());
     
     llvm::PointerType* ptrType = llvm::PointerType::get(Int8Ty, 0);
     llvm::GlobalVariable* globalStr = new llvm::GlobalVariable(CGM.getModule(), constStr->getType(), true, llvm::GlobalValue::PrivateLinkage, constStr);
     llvm::Value* StrPtr = Builder.CreatePointerCast(globalStr, ptrType);
     SourceManager &SM = CGM.getContext().getSourceManager();
     int lineNo = SM.getPresumedLoc(VoteLoc).getLine();
#if 1
     if (!HaveInsertPoint())
       return;
     // Build call __ft_vote(&loc, var, size)
     std::string str("__ft_vote");
     if (whichSide & 0x1) str += "r";
     else str += "l";
     if (whichSide & 0x2) str += "_atomic";
     if (isVarIncluded(VoteVar, AutoSize) >= 0)
       str += "_auto";
     if (whichSide & 0x8) str = "__ft_votenow";
     const char *LibCallName = str.c_str();
     
     if (!isDebug) {
       llvm::Type *Params[] = {AddrPtr->getType(), CGM.Int32Ty};
       llvm::Value *Args[] = {
         AddrPtr,
         Builder.CreateIntCast(TSize, Int32Ty, /*isSigned*/ true)
         };
       auto *FTy = llvm::FunctionType::get(CGM.Int32Ty, Params, /*isVarArg=*/false);
       llvm::FunctionCallee Func = CGM.CreateRuntimeFunction(FTy, LibCallName);
       EmitRuntimeCall(Func, Args);
     } else {
       llvm::Type *Params[] = {AddrPtr->getType(), CGM.Int32Ty, CGM.VoidPtrTy, CGM.Int32Ty};
       llvm::Value *Args[] = {
         AddrPtr,
         Builder.CreateIntCast(TSize, Int32Ty, /*isSigned*/ true),
	 StrPtr,
         Builder.CreateIntCast(llvm::ConstantInt::get(Int32Ty, lineNo), Int32Ty, /*isSigned*/ true)
         };
       auto *FTy = llvm::FunctionType::get(CGM.Int32Ty, Params, /*isVarArg=*/false);
       llvm::FunctionCallee Func = CGM.CreateRuntimeFunction(FTy, LibCallName);
       EmitRuntimeCall(Func, Args);
     }
#else
     if (isVarIncluded(VoteVar, AutoSize) >= 0)	// set AUTO if it is
       whichSide = whichSide | 0x4;
     CGM.getFTRuntime().emitVoteCall(*this, AddrPtr, sizeOfType, whichSide);
//     CGM.getFTRuntime().emitVoteCall_debug(CGF, AddrPtr, sizeOfType, StrPtr, lineNo, whichSide);
#endif
     if (keep_status) return;
     VoteNow = false;
     VoteVar = nullptr;
}
