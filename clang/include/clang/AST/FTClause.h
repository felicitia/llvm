//===- FTClause.h - Classes for FT clauses --------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// This file defines FT AST classes for clauses.
/// There are clauses for executable directives, clauses for declarative
/// directives and clauses which can be used in both kinds of directives.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_FTCLAUSE_H
#define LLVM_CLANG_AST_FTCLAUSE_H

#include "clang/AST/ASTFwd.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Expr.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtIterator.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/FTKinds.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
//#include "llvm/Frontend/OpenMP/OMPAssume.h"
//#include "llvm/Frontend/OpenMP/OMPConstants.h"
//#include "llvm/Frontend/OpenMP/OMPContext.h"
#include "llvm/Frontend/FT/FTConstants.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/TrailingObjects.h"
#include <cassert>
#include <cstddef>
#include <iterator>
#include <utility>

namespace clang {
class ASTContext;

//===----------------------------------------------------------------------===//
// AST classes for clauses.
//===----------------------------------------------------------------------===//

/// This is a basic class for representing single FT clause.
class FTClause {
  /// Starting location of the clause (the clause keyword).
  SourceLocation StartLoc;

  /// Ending location of the clause.
  SourceLocation EndLoc;

  /// Kind of the clause.
  FTClauseKind Kind;

protected:
  FTClause(FTClauseKind K, SourceLocation StartLoc, SourceLocation EndLoc)
      : StartLoc(StartLoc), EndLoc(EndLoc), Kind(K) {}

public:
  /// Returns the starting location of the clause.
  SourceLocation getBeginLoc() const { return StartLoc; }

  /// Returns the ending location of the clause.
  SourceLocation getEndLoc() const { return EndLoc; }

  /// Sets the starting location of the clause.
  void setLocStart(SourceLocation Loc) { StartLoc = Loc; }

  /// Sets the ending location of the clause.
  void setLocEnd(SourceLocation Loc) { EndLoc = Loc; }

  /// Returns kind of FT clause (private, shared, reduction, etc.).
  FTClauseKind getClauseKind() const { return Kind; }

  bool isImplicit() const { return StartLoc.isInvalid(); }

  using child_iterator = StmtIterator;
  using const_child_iterator = ConstStmtIterator;
  using child_range = llvm::iterator_range<child_iterator>;
  using const_child_range = llvm::iterator_range<const_child_iterator>;

  child_range children();
  const_child_range children() const {
    auto Children = const_cast<FTClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  /// Get the iterator range for the expressions used in the clauses. Used
  /// expressions include only the children that must be evaluated at the
  /// runtime before entering the construct.
  child_range used_children();
  const_child_range used_children() const {
    auto Children = const_cast<FTClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  static bool classof(const FTClause *) { return true; }
};

/// This structure contains most locations needed for by an FTVarListClause.
struct FTVarListLocTy {
  /// Starting location of the clause (the clause keyword).
  SourceLocation StartLoc;
  /// Location of '('.
  SourceLocation LParenLoc;
  /// Ending location of the clause.
  SourceLocation EndLoc;
  FTVarListLocTy() = default;
  FTVarListLocTy(SourceLocation StartLoc, SourceLocation LParenLoc,
                  SourceLocation EndLoc)
      : StartLoc(StartLoc), LParenLoc(LParenLoc), EndLoc(EndLoc) {}
};

/// This represents clauses with the list of variables like 'private',
/// 'firstprivate', 'copyin', 'shared', or 'reduction' clauses in the
/// '#pragma omp ...' directives.
template <class T> class FTVarListClause : public FTClause {
  friend class FTClauseReader;

  /// Location of '('.
  SourceLocation LParenLoc;

  /// Number of variables in the list.
  unsigned NumVars;

protected:
  /// Build a clause with \a N variables
  ///
  /// \param K Kind of the clause.
  /// \param StartLoc Starting location of the clause (the clause keyword).
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  FTVarListClause(FTClauseKind K, SourceLocation StartLoc,
                   SourceLocation LParenLoc, SourceLocation EndLoc, unsigned N)
      : FTClause(K, StartLoc, EndLoc), LParenLoc(LParenLoc), NumVars(N) {}

  /// Fetches list of variables associated with this clause.
  MutableArrayRef<Expr *> getVarRefs() {
    return MutableArrayRef<Expr *>(
        static_cast<T *>(this)->template getTrailingObjects<Expr *>(), NumVars);
  }

  /// Sets the list of variables for this clause.
  void setVarRefs(ArrayRef<Expr *> VL) {
    assert(VL.size() == NumVars &&
           "Number of variables is not the same as the preallocated buffer");
    std::copy(VL.begin(), VL.end(),
              static_cast<T *>(this)->template getTrailingObjects<Expr *>());
  }

public:
  using varlist_iterator = MutableArrayRef<Expr *>::iterator;
  using varlist_const_iterator = ArrayRef<const Expr *>::iterator;
  using varlist_range = llvm::iterator_range<varlist_iterator>;
  using varlist_const_range = llvm::iterator_range<varlist_const_iterator>;

  unsigned varlist_size() const { return NumVars; }
  bool varlist_empty() const { return NumVars == 0; }

  varlist_range varlists() {
    return varlist_range(varlist_begin(), varlist_end());
  }
  varlist_const_range varlists() const {
    return varlist_const_range(varlist_begin(), varlist_end());
  }

  varlist_iterator varlist_begin() { return getVarRefs().begin(); }
  varlist_iterator varlist_end() { return getVarRefs().end(); }
  varlist_const_iterator varlist_begin() const { return getVarRefs().begin(); }
  varlist_const_iterator varlist_end() const { return getVarRefs().end(); }

  /// Sets the location of '('.
  void setLParenLoc(SourceLocation Loc) { LParenLoc = Loc; }

  /// Returns the location of '('.
  SourceLocation getLParenLoc() const { return LParenLoc; }

  /// Fetches list of all variables in the clause.
  ArrayRef<const Expr *> getVarRefs() const {
    return llvm::ArrayRef(
        static_cast<const T *>(this)->template getTrailingObjects<Expr *>(),
        NumVars);
  }

};

class FTLhsClause final
    : public FTVarListClause<FTLhsClause>,
      private llvm::TrailingObjects<FTLhsClause, Expr *> {
  friend FTVarListClause;
  friend TrailingObjects;

  /// Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  FTLhsClause(SourceLocation StartLoc, SourceLocation LParenLoc,
                  SourceLocation EndLoc, unsigned N)
      : FTVarListClause<FTLhsClause>(llvm::ft::FTC_lhs, StartLoc,
                                          LParenLoc, EndLoc, N) {}

  /// Build an empty clause.
  ///
  /// \param N Number of variables.
  explicit FTLhsClause(unsigned N)
      : FTVarListClause<FTLhsClause>(llvm::ft::FTC_lhs,
                                          SourceLocation(), SourceLocation(),
                                          SourceLocation(), N) {}

public:
  void setVarSizeRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  void setVarSizePtrRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL, ArrayRef<Expr *> PL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    assert (VL.size() == PL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
      STL.push_back(PL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  /// Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  static FTLhsClause *Create(const ASTContext &C, SourceLocation StartLoc,
                                 SourceLocation LParenLoc,
                                 SourceLocation EndLoc, ArrayRef<Expr *> VL,
                                 ArrayRef<Expr *> SL, ArrayRef<Expr *> PL);

  /// Creates an empty clause with \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  static FTLhsClause *CreateEmpty(const ASTContext &C, unsigned N);

  child_range children() {
    return child_range(reinterpret_cast<Stmt **>(varlist_begin()),
                       reinterpret_cast<Stmt **>(varlist_end()));
  }

  const_child_range children() const {
    auto Children = const_cast<FTLhsClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const FTClause *T) {
    return T->getClauseKind() == llvm::ft::FTC_lhs;
  }
};

class FTNolhsClause final
    : public FTVarListClause<FTNolhsClause>,
      private llvm::TrailingObjects<FTNolhsClause, Expr *> {
  friend FTVarListClause;
  friend TrailingObjects;

  /// Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  FTNolhsClause(SourceLocation StartLoc, SourceLocation LParenLoc,
                  SourceLocation EndLoc, unsigned N)
      : FTVarListClause<FTNolhsClause>(llvm::ft::FTC_nolhs, StartLoc,
                                          LParenLoc, EndLoc, N) {}

  /// Build an empty clause.
  ///
  /// \param N Number of variables.
  explicit FTNolhsClause(unsigned N)
      : FTVarListClause<FTNolhsClause>(llvm::ft::FTC_nolhs,
                                          SourceLocation(), SourceLocation(),
                                          SourceLocation(), N) {}

public:
  void setVarSizeRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Novariable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  void setVarSizePtrRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL, ArrayRef<Expr *> PL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    assert (VL.size() == PL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
      STL.push_back(PL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  /// Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  static FTNolhsClause *Create(const ASTContext &C, SourceLocation StartLoc,
                                 SourceLocation LParenLoc,
                                 SourceLocation EndLoc, ArrayRef<Expr *> VL,
                                 ArrayRef<Expr *> SL, ArrayRef<Expr *> PL);

  /// Creates an empty clause with \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  static FTNolhsClause *CreateEmpty(const ASTContext &C, unsigned N);

  child_range children() {
    return child_range(reinterpret_cast<Stmt **>(varlist_begin()),
                       reinterpret_cast<Stmt **>(varlist_end()));
  }

  const_child_range children() const {
    auto Children = const_cast<FTNolhsClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const FTClause *T) {
    return T->getClauseKind() == llvm::ft::FTC_nolhs;
  }
};

class FTRhsClause final
    : public FTVarListClause<FTRhsClause>,
      private llvm::TrailingObjects<FTRhsClause, Expr *> {
  friend FTVarListClause;
  friend TrailingObjects;

  /// Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  FTRhsClause(SourceLocation StartLoc, SourceLocation LParenLoc,
                  SourceLocation EndLoc, unsigned N)
      : FTVarListClause<FTRhsClause>(llvm::ft::FTC_rhs, StartLoc,
                                          LParenLoc, EndLoc, N) {}

  /// Build an empty clause.
  ///
  /// \param N Number of variables.
  explicit FTRhsClause(unsigned N)
      : FTVarListClause<FTRhsClause>(llvm::ft::FTC_rhs,
                                          SourceLocation(), SourceLocation(),
                                          SourceLocation(), N) {}

public:
  void setVarSizeRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  void setVarSizePtrRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL, ArrayRef<Expr *> PL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    assert (VL.size() == PL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
      STL.push_back(PL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  /// Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  static FTRhsClause *Create(const ASTContext &C, SourceLocation StartLoc,
                                 SourceLocation LParenLoc,
                                 SourceLocation EndLoc, ArrayRef<Expr *> VL,
                                 ArrayRef<Expr *> SL, ArrayRef<Expr *> PL);

  /// Creates an empty clause with \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  static FTRhsClause *CreateEmpty(const ASTContext &C, unsigned N);

  child_range children() {
    return child_range(reinterpret_cast<Stmt **>(varlist_begin()),
                       reinterpret_cast<Stmt **>(varlist_end()));
  }

  const_child_range children() const {
    auto Children = const_cast<FTRhsClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const FTClause *T) {
    return T->getClauseKind() == llvm::ft::FTC_rhs;
  }
};

class FTNorhsClause final
    : public FTVarListClause<FTNorhsClause>,
      private llvm::TrailingObjects<FTNorhsClause, Expr *> {
  friend FTVarListClause;
  friend TrailingObjects;

  /// Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  FTNorhsClause(SourceLocation StartLoc, SourceLocation LParenLoc,
                  SourceLocation EndLoc, unsigned N)
      : FTVarListClause<FTNorhsClause>(llvm::ft::FTC_norhs, StartLoc,
                                          LParenLoc, EndLoc, N) {}

  /// Build an empty clause.
  ///
  /// \param N Number of variables.
  explicit FTNorhsClause(unsigned N)
      : FTVarListClause<FTNorhsClause>(llvm::ft::FTC_norhs,
                                          SourceLocation(), SourceLocation(),
                                          SourceLocation(), N) {}

public:
  void setVarSizeRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  void setVarSizePtrRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL, ArrayRef<Expr *> PL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    assert (VL.size() == PL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
      STL.push_back(PL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  /// Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  static FTNorhsClause *Create(const ASTContext &C, SourceLocation StartLoc,
                                 SourceLocation LParenLoc,
                                 SourceLocation EndLoc, ArrayRef<Expr *> VL,
                                 ArrayRef<Expr *> SL, ArrayRef<Expr *> PL);

  /// Creates an empty clause with \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  static FTNorhsClause *CreateEmpty(const ASTContext &C, unsigned N);

  child_range children() {
    return child_range(reinterpret_cast<Stmt **>(varlist_begin()),
                       reinterpret_cast<Stmt **>(varlist_end()));
  }

  const_child_range children() const {
    auto Children = const_cast<FTNorhsClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const FTClause *T) {
    return T->getClauseKind() == llvm::ft::FTC_norhs;
  }
};

class FTNovoteClause final
    : public FTVarListClause<FTNovoteClause>,
      private llvm::TrailingObjects<FTNovoteClause, Expr *> {
  friend FTVarListClause;
  friend TrailingObjects;

  /// Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  FTNovoteClause(SourceLocation StartLoc, SourceLocation LParenLoc,
                  SourceLocation EndLoc, unsigned N)
      : FTVarListClause<FTNovoteClause>(llvm::ft::FTC_novote, StartLoc,
                                          LParenLoc, EndLoc, N) {}

  /// Build an empty clause.
  ///
  /// \param N Number of variables.
  explicit FTNovoteClause(unsigned N)
      : FTVarListClause<FTNovoteClause>(llvm::ft::FTC_novote,
                                          SourceLocation(), SourceLocation(),
                                          SourceLocation(), N) {}

public:
  void setVarSizeRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  void setVarSizePtrRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL, ArrayRef<Expr *> PL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    assert (VL.size() == PL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
      STL.push_back(PL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  /// Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  static FTNovoteClause *Create(const ASTContext &C, SourceLocation StartLoc,
                                 SourceLocation LParenLoc,
                                 SourceLocation EndLoc, ArrayRef<Expr *> VL,
                                 ArrayRef<Expr *> SL, ArrayRef<Expr *> PL);

  /// Creates an empty clause with \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  static FTNovoteClause *CreateEmpty(const ASTContext &C, unsigned N);

  child_range children() {
    return child_range(reinterpret_cast<Stmt **>(varlist_begin()),
                       reinterpret_cast<Stmt **>(varlist_end()));
  }

  const_child_range children() const {
    auto Children = const_cast<FTNovoteClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const FTClause *T) {
    return T->getClauseKind() == llvm::ft::FTC_novote;
  }
};

class FTVoteClause final
    : public FTVarListClause<FTVoteClause>,
      private llvm::TrailingObjects<FTVoteClause, Expr *> {
  friend FTVarListClause;
  friend TrailingObjects;

  /// Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  FTVoteClause(SourceLocation StartLoc, SourceLocation LParenLoc,
                  SourceLocation EndLoc, unsigned N)
      : FTVarListClause<FTVoteClause>(llvm::ft::FTC_vote, StartLoc,
                                          LParenLoc, EndLoc, N) {}

  /// Build an empty clause.
  ///
  /// \param N Number of variables.
  explicit FTVoteClause(unsigned N)
      : FTVarListClause<FTVoteClause>(llvm::ft::FTC_vote,
                                          SourceLocation(), SourceLocation(),
                                          SourceLocation(), N) {}

public:
  void setVarSizeRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  void setVarSizePtrRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL, ArrayRef<Expr *> PL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    assert (VL.size() == PL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
      STL.push_back(PL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  /// Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  static FTVoteClause *Create(const ASTContext &C, SourceLocation StartLoc,
                                 SourceLocation LParenLoc,
                                 SourceLocation EndLoc, ArrayRef<Expr *> VL,
                                 ArrayRef<Expr *> SL, ArrayRef<Expr *> PL);

  /// Creates an empty clause with \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  static FTVoteClause *CreateEmpty(const ASTContext &C, unsigned N);

  child_range children() {
    return child_range(reinterpret_cast<Stmt **>(varlist_begin()),
                       reinterpret_cast<Stmt **>(varlist_end()));
  }

  const_child_range children() const {
    auto Children = const_cast<FTVoteClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const FTClause *T) {
    return T->getClauseKind() == llvm::ft::FTC_vote;
  }
};

class FTAutoClause final
    : public FTVarListClause<FTAutoClause>,
      private llvm::TrailingObjects<FTAutoClause, Expr *> {
  friend FTVarListClause;
  friend TrailingObjects;

  /// Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  FTAutoClause(SourceLocation StartLoc, SourceLocation LParenLoc,
                  SourceLocation EndLoc, unsigned N)
      : FTVarListClause<FTAutoClause>(llvm::ft::FTC_auto, StartLoc,
                                          LParenLoc, EndLoc, N) {}

  /// Build an empty clause.
  ///
  /// \param N Number of variables.
  explicit FTAutoClause(unsigned N)
      : FTVarListClause<FTAutoClause>(llvm::ft::FTC_auto,
                                          SourceLocation(), SourceLocation(),
                                          SourceLocation(), N) {}

public:
  void setVarSizeRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  void setVarSizePtrRefs(ArrayRef<Expr *> VL, ArrayRef<Expr *> SL, ArrayRef<Expr *> PL) {
    std::vector<Expr *> STL;
    assert (VL.size() == SL.size() && "Variable and Size array length mismatch!!");
    assert (VL.size() == PL.size() && "Variable and Size array length mismatch!!");
    for (int i = 0; i < (int)VL.size(); i++) {
      STL.push_back(VL[i]);
      STL.push_back(SL[i]);
      STL.push_back(PL[i]);
    }
    ArrayRef<Expr *> TL(STL);
    std::copy(TL.begin(), TL.end(),
              /* static_cast<T *>(this)->template */ getTrailingObjects<Expr *>());
  }
  /// Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the clause.
  /// \param LParenLoc Location of '('.
  /// \param EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  static FTAutoClause *Create(const ASTContext &C, SourceLocation StartLoc,
                                 SourceLocation LParenLoc,
                                 SourceLocation EndLoc, ArrayRef<Expr *> VL,
                                 ArrayRef<Expr *> SL, ArrayRef<Expr *> PL);

  /// Creates an empty clause with \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  static FTAutoClause *CreateEmpty(const ASTContext &C, unsigned N);

  child_range children() {
    return child_range(reinterpret_cast<Stmt **>(varlist_begin()),
                       reinterpret_cast<Stmt **>(varlist_end()));
  }

  const_child_range children() const {
    auto Children = const_cast<FTAutoClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const FTClause *T) {
    return T->getClauseKind() == llvm::ft::FTC_auto;
  }
}
;
/// This class implements a simple visitor for FTClause
/// subclasses.
template<class ImplClass, template <typename> class Ptr, typename RetTy>
class FTClauseVisitorBase {
public:
#define PTR(CLASS) Ptr<CLASS>
#define DISPATCH(CLASS) \
  return static_cast<ImplClass*>(this)->Visit##CLASS(static_cast<PTR(CLASS)>(S))

#define GEN_CLANG_CLAUSE_CLASS
#define CLAUSE_CLASS(Enum, Str, Class)                                         \
  RetTy Visit##Class(PTR(Class) S) { DISPATCH(Class); }
#include "llvm/Frontend/FT/FT.inc"

  RetTy Visit(PTR(FTClause) S) {
    // Top switch clause: visit each FTClause.
    switch (S->getClauseKind()) {
#define GEN_CLANG_CLAUSE_CLASS
#define CLAUSE_CLASS(Enum, Str, Class)                                         \
  case llvm::ft::Clause::Enum:                                                \
    return Visit##Class(static_cast<PTR(Class)>(S));
#include "llvm/Frontend/FT/FT.inc"
    }
  }
  // Base case, ignore it. :)
  RetTy VisitFTClause(PTR(FTClause) Node) { return RetTy(); }
#undef PTR
#undef DISPATCH
};

template <typename T> using const_ptr = std::add_pointer_t<std::add_const_t<T>>;

template <class ImplClass, typename RetTy = void>
class FTClauseVisitor
    : public FTClauseVisitorBase<ImplClass, std::add_pointer_t, RetTy> {};
template<class ImplClass, typename RetTy = void>
class ConstFTClauseVisitor :
      public FTClauseVisitorBase <ImplClass, const_ptr, RetTy> {};

class FTClausePrinter final : public FTClauseVisitor<FTClausePrinter> {
  raw_ostream &OS;
  const PrintingPolicy &Policy;

  /// Process clauses with list of variables.
  template <typename T> void VisitFTClauseList(T *Node, char StartSym);
  /// Process motion clauses.
  template <typename T> void VisitFTMotionClause(T *Node);

public:
  FTClausePrinter(raw_ostream &OS, const PrintingPolicy &Policy)
      : OS(OS), Policy(Policy) {}

#define GEN_CLANG_CLAUSE_CLASS
#define CLAUSE_CLASS(Enum, Str, Class) void Visit##Class(Class *S);
#include "llvm/Frontend/FT/FT.inc"
};

class FTChildren final
    : private llvm::TrailingObjects<FTChildren, FTClause *, Stmt *> {
  friend TrailingObjects;
  friend class FTClauseReader;
  friend class FTExecutableDirective;
  template <typename T> friend class FTDeclarativeDirective;

  /// Numbers of clauses.
  unsigned NumClauses = 0;
  /// Number of child expressions/stmts.
  unsigned NumChildren = 0;
  /// true if the directive has associated statement.
  bool HasAssociatedStmt = false;

  /// Define the sizes of each trailing object array except the last one. This
  /// is required for TrailingObjects to work properly.
  size_t numTrailingObjects(OverloadToken<FTClause *>) const {
    return NumClauses;
  }

  FTChildren() = delete;

  FTChildren(unsigned NumClauses, unsigned NumChildren, bool HasAssociatedStmt)
      : NumClauses(NumClauses), NumChildren(NumChildren),
        HasAssociatedStmt(HasAssociatedStmt) {}

  static size_t size(unsigned NumClauses, bool HasAssociatedStmt,
                     unsigned NumChildren);

  static FTChildren *Create(void *Mem, ArrayRef<FTClause *> Clauses);
  static FTChildren *Create(void *Mem, ArrayRef<FTClause *> Clauses, Stmt *S,
                             unsigned NumChildren = 0);
  static FTChildren *CreateEmpty(void *Mem, unsigned NumClauses,
                                  bool HasAssociatedStmt = false,
                                  unsigned NumChildren = 0);

public:
  unsigned getNumClauses() const { return NumClauses; }
  unsigned getNumChildren() const { return NumChildren; }
  bool hasAssociatedStmt() const { return HasAssociatedStmt; }

  /// Set associated statement.
  void setAssociatedStmt(Stmt *S) {
    getTrailingObjects<Stmt *>()[NumChildren] = S;
  }

  void setChildren(ArrayRef<Stmt *> Children);

  /// Sets the list of variables for this clause.
  ///
  /// \param Clauses The list of clauses for the directive.
  ///
  void setClauses(ArrayRef<FTClause *> Clauses);

  /// Returns statement associated with the directive.
  const Stmt *getAssociatedStmt() const {
    return const_cast<FTChildren *>(this)->getAssociatedStmt();
  }
  Stmt *getAssociatedStmt() {
    assert(HasAssociatedStmt &&
           "Expected directive with the associated statement.");
    return getTrailingObjects<Stmt *>()[NumChildren];
  }

  /// Get the clauses storage.
  MutableArrayRef<FTClause *> getClauses() {
    return llvm::MutableArrayRef(getTrailingObjects<FTClause *>(),
                                     NumClauses);
  }
  ArrayRef<FTClause *> getClauses() const {
    return const_cast<FTChildren *>(this)->getClauses();
  }

  /// Returns the captured statement associated with the
  /// component region within the (combined) directive.
  ///
  /// \param RegionKind Component region kind.
  const CapturedStmt *
  getCapturedStmt(FTDirectiveKind RegionKind,
                  ArrayRef<FTDirectiveKind> CaptureRegions) const {
    assert(llvm::any_of(
               CaptureRegions,
               [=](const FTDirectiveKind K) { return K == RegionKind; }) &&
           "RegionKind not found in FT CaptureRegions.");
    auto *CS = cast<CapturedStmt>(getAssociatedStmt());
    for (auto ThisCaptureRegion : CaptureRegions) {
      if (ThisCaptureRegion == RegionKind)
        return CS;
      CS = cast<CapturedStmt>(CS->getCapturedStmt());
    }
    llvm_unreachable("Incorrect RegionKind specified for directive.");
  }

  /// Get innermost captured statement for the construct.
  CapturedStmt *
  getInnermostCapturedStmt(ArrayRef<FTDirectiveKind> CaptureRegions) {
    assert(hasAssociatedStmt() && "Must have associated captured statement.");
    assert(!CaptureRegions.empty() &&
           "At least one captured statement must be provided.");
    auto *CS = cast<CapturedStmt>(getAssociatedStmt());
    for (unsigned Level = CaptureRegions.size(); Level > 1; --Level)
      CS = cast<CapturedStmt>(CS->getCapturedStmt());
    return CS;
  }

  const CapturedStmt *
  getInnermostCapturedStmt(ArrayRef<FTDirectiveKind> CaptureRegions) const {
    return const_cast<FTChildren *>(this)->getInnermostCapturedStmt(
        CaptureRegions);
  }

  MutableArrayRef<Stmt *> getChildren();
  ArrayRef<Stmt *> getChildren() const {
    return const_cast<FTChildren *>(this)->getChildren();
  }

  Stmt *getRawStmt() {
    assert(HasAssociatedStmt &&
           "Expected directive with the associated statement.");
    if (auto *CS = dyn_cast<CapturedStmt>(getAssociatedStmt())) {
      Stmt *S = nullptr;
      do {
        S = CS->getCapturedStmt();
        CS = dyn_cast<CapturedStmt>(S);
      } while (CS);
      return S;
    }
    return getAssociatedStmt();
  }
  const Stmt *getRawStmt() const {
    return const_cast<FTChildren *>(this)->getRawStmt();
  }

  Stmt::child_range getAssociatedStmtAsRange() {
    if (!HasAssociatedStmt)
      return Stmt::child_range(Stmt::child_iterator(), Stmt::child_iterator());
    return Stmt::child_range(&getTrailingObjects<Stmt *>()[NumChildren],
                             &getTrailingObjects<Stmt *>()[NumChildren + 1]);
  }
};


} // namespace clang

#endif // LLVM_CLANG_AST_FTCLAUSE_H
