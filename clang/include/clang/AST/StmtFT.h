//===- StmtFT.h - Classes for FT directives  ------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file defines FT AST classes for executable directives and
/// clauses.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_STMTFT_H
#define LLVM_CLANG_AST_STMTFT_H

#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/FTClause.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtCXX.h"
#include "clang/Basic/FTKinds.h"
#include "clang/Basic/SourceLocation.h"


namespace clang {

class FTExecutableDirective : public Stmt {
  friend class ASTStmtReader;
  friend class ASTStmtWriter;

  /// Kind of the directive.
  FTDirectiveKind Kind = llvm::ft::FTD_unknown;
  /// Starting location of the directive (directive keyword).
  SourceLocation StartLoc;
  /// Ending location of the directive.
  SourceLocation EndLoc;

  /// Get the clauses storage.
  MutableArrayRef<FTClause *> getClauses() {
    if (!Data)
      return std::nullopt;
    return Data->getClauses();
  }

protected:
  /// Data, associated with the directive.
  FTChildren *Data = nullptr;

  /// Build instance of directive of class \a K.
  ///
  /// \param SC Statement class.
  /// \param K Kind of FT directive.
  /// \param StartLoc Starting location of the directive (directive keyword).
  /// \param EndLoc Ending location of the directive.
  ///
  FTExecutableDirective(StmtClass SC, FTDirectiveKind K,
                         SourceLocation StartLoc, SourceLocation EndLoc)
      : Stmt(SC), Kind(K), StartLoc(std::move(StartLoc)),
        EndLoc(std::move(EndLoc)) {}

  template <typename T, typename... Params>
  static T *createDirective(const ASTContext &C, ArrayRef<FTClause *> Clauses,
                            Stmt *AssociatedStmt, unsigned NumChildren,
                            Params &&... P) {
    void *Mem =
        C.Allocate(sizeof(T) + FTChildren::size(Clauses.size(), AssociatedStmt,
                                                 NumChildren),
                   alignof(T));

    auto *Data = FTChildren::Create(reinterpret_cast<T *>(Mem) + 1, Clauses,
                                     AssociatedStmt, NumChildren);
    auto *Inst = new (Mem) T(std::forward<Params>(P)...);
    Inst->Data = Data;
    return Inst;
  }

  template <typename T, typename... Params>
  static T *createEmptyDirective(const ASTContext &C, unsigned NumClauses,
                                 bool HasAssociatedStmt, unsigned NumChildren,
                                 Params &&... P) {
    void *Mem =
        C.Allocate(sizeof(T) + FTChildren::size(NumClauses, HasAssociatedStmt,
                                                 NumChildren),
                   alignof(T));
    auto *Data =
        FTChildren::CreateEmpty(reinterpret_cast<T *>(Mem) + 1, NumClauses,
                                 HasAssociatedStmt, NumChildren);
    auto *Inst = new (Mem) T(std::forward<Params>(P)...);
    Inst->Data = Data;
    return Inst;
  }

  template <typename T>
  static T *createEmptyDirective(const ASTContext &C, unsigned NumClauses,
                                 bool HasAssociatedStmt = false,
                                 unsigned NumChildren = 0) {
    void *Mem =
        C.Allocate(sizeof(T) + FTChildren::size(NumClauses, HasAssociatedStmt,
                                                 NumChildren),
                   alignof(T));
    auto *Data =
        FTChildren::CreateEmpty(reinterpret_cast<T *>(Mem) + 1, NumClauses,
                                 HasAssociatedStmt, NumChildren);
    auto *Inst = new (Mem) T;
    Inst->Data = Data;
    return Inst;
  }

public:
  /// Iterates over expressions/statements used in the construct.
  class used_clauses_child_iterator
      : public llvm::iterator_adaptor_base<
            used_clauses_child_iterator, ArrayRef<FTClause *>::iterator,
            std::forward_iterator_tag, Stmt *, ptrdiff_t, Stmt *, Stmt *> {
    ArrayRef<FTClause *>::iterator End;
    FTClause::child_iterator ChildI, ChildEnd;

    void MoveToNext() {
      if (ChildI != ChildEnd)
        return;
      while (this->I != End) {
        ++this->I;
        if (this->I != End) {
          ChildI = (*this->I)->used_children().begin();
          ChildEnd = (*this->I)->used_children().end();
          if (ChildI != ChildEnd)
            return;
        }
      }
    }

  public:
    explicit used_clauses_child_iterator(ArrayRef<FTClause *> Clauses)
        : used_clauses_child_iterator::iterator_adaptor_base(Clauses.begin()),
          End(Clauses.end()) {
      if (this->I != End) {
        ChildI = (*this->I)->used_children().begin();
        ChildEnd = (*this->I)->used_children().end();
        MoveToNext();
      }
    }
    Stmt *operator*() const { return *ChildI; }
    Stmt *operator->() const { return **this; }

    used_clauses_child_iterator &operator++() {
      ++ChildI;
      if (ChildI != ChildEnd)
        return *this;
      if (this->I != End) {
        ++this->I;
        if (this->I != End) {
          ChildI = (*this->I)->used_children().begin();
          ChildEnd = (*this->I)->used_children().end();
        }
      }
      MoveToNext();
      return *this;
    }
  };

  static llvm::iterator_range<used_clauses_child_iterator>
  used_clauses_children(ArrayRef<FTClause *> Clauses) {
    return {used_clauses_child_iterator(Clauses),
            used_clauses_child_iterator(llvm::ArrayRef(Clauses.end(), (size_t)0))};
  }

  /// Iterates over a filtered subrange of clauses applied to a
  /// directive.
  ///
  /// This iterator visits only clauses of type SpecificClause.
  template <typename SpecificClause>
  class specific_clause_iterator
      : public llvm::iterator_adaptor_base<
            specific_clause_iterator<SpecificClause>,
            ArrayRef<FTClause *>::const_iterator, std::forward_iterator_tag,
            const SpecificClause *, ptrdiff_t, const SpecificClause *,
            const SpecificClause *> {
    ArrayRef<FTClause *>::const_iterator End;

    void SkipToNextClause() {
      while (this->I != End && !isa<SpecificClause>(*this->I))
        ++this->I;
    }

  public:
    explicit specific_clause_iterator(ArrayRef<FTClause *> Clauses)
        : specific_clause_iterator::iterator_adaptor_base(Clauses.begin()),
          End(Clauses.end()) {
      SkipToNextClause();
    }

    const SpecificClause *operator*() const {
      return cast<SpecificClause>(*this->I);
    }
    const SpecificClause *operator->() const { return **this; }

    specific_clause_iterator &operator++() {
      ++this->I;
      SkipToNextClause();
      return *this;
    }
  };

  template <typename SpecificClause>
  static llvm::iterator_range<specific_clause_iterator<SpecificClause>>
  getClausesOfKind(ArrayRef<FTClause *> Clauses) {
    return {specific_clause_iterator<SpecificClause>(Clauses),
            specific_clause_iterator<SpecificClause>(
                llvm::ArrayRef(Clauses.end(), (size_t)0))};
  }

  template <typename SpecificClause>
  llvm::iterator_range<specific_clause_iterator<SpecificClause>>
  getClausesOfKind() const {
    return getClausesOfKind<SpecificClause>(clauses());
  }

  /// Gets a single clause of the specified kind associated with the
  /// current directive iff there is only one clause of this kind (and assertion
  /// is fired if there is more than one clause is associated with the
  /// directive). Returns nullptr if no clause of this kind is associated with
  /// the directive.
  template <typename SpecificClause>
  static const SpecificClause *getSingleClause(ArrayRef<FTClause *> Clauses) {
    auto ClausesOfKind = getClausesOfKind<SpecificClause>(Clauses);

    if (ClausesOfKind.begin() != ClausesOfKind.end()) {
      assert(std::next(ClausesOfKind.begin()) == ClausesOfKind.end() &&
             "There are at least 2 clauses of the specified kind");
      return *ClausesOfKind.begin();
    }
    return nullptr;
  }

  template <typename SpecificClause>
  const SpecificClause *getSingleClause() const {
    return getSingleClause<SpecificClause>(clauses());
  }

  /// Returns true if the current directive has one or more clauses of a
  /// specific kind.
  template <typename SpecificClause>
  bool hasClausesOfKind() const {
    auto Clauses = getClausesOfKind<SpecificClause>();
    return Clauses.begin() != Clauses.end();
  }

  /// Returns starting location of directive kind.
  SourceLocation getBeginLoc() const { return StartLoc; }
  /// Returns ending location of directive.
  SourceLocation getEndLoc() const { return EndLoc; }

  /// Set starting location of directive kind.
  ///
  /// \param Loc New starting location of directive.
  ///
  void setLocStart(SourceLocation Loc) { StartLoc = Loc; }
  /// Set ending location of directive.
  ///
  /// \param Loc New ending location of directive.
  ///
  void setLocEnd(SourceLocation Loc) { EndLoc = Loc; }

  /// Get number of clauses.
  unsigned getNumClauses() const {
    if (!Data)
      return 0;
    return Data->getNumClauses();
  }

  /// Returns specified clause.
  ///
  /// \param I Number of clause.
  ///
  FTClause *getClause(unsigned I) const { return clauses()[I]; }

  /// Returns true if directive has associated statement.
  bool hasAssociatedStmt() const { return Data && Data->hasAssociatedStmt(); }

  /// Returns statement associated with the directive.
  const Stmt *getAssociatedStmt() const {
    return const_cast<FTExecutableDirective *>(this)->getAssociatedStmt();
  }
  Stmt *getAssociatedStmt() {
    assert(hasAssociatedStmt() &&
           "Expected directive with the associated statement.");
    return Data->getAssociatedStmt();
  }

  /// Returns the captured statement associated with the
  /// component region within the (combined) directive.
  ///
  /// \param RegionKind Component region kind.
  const CapturedStmt *getCapturedStmt(FTDirectiveKind RegionKind) const {
    assert(hasAssociatedStmt() &&
           "Expected directive with the associated statement.");
    SmallVector<FTDirectiveKind, 4> CaptureRegions;
    getFTCaptureRegions(CaptureRegions, getDirectiveKind());
    return Data->getCapturedStmt(RegionKind, CaptureRegions);
  }

  /// Get innermost captured statement for the construct.
  CapturedStmt *getInnermostCapturedStmt() {
    assert(hasAssociatedStmt() &&
           "Expected directive with the associated statement.");
    SmallVector<FTDirectiveKind, 4> CaptureRegions;
    getFTCaptureRegions(CaptureRegions, getDirectiveKind());
    return Data->getInnermostCapturedStmt(CaptureRegions);
  }

  const CapturedStmt *getInnermostCapturedStmt() const {
    return const_cast<FTExecutableDirective *>(this)
        ->getInnermostCapturedStmt();
  }

  FTDirectiveKind getDirectiveKind() const { return Kind; }

  static bool classof(const Stmt *S) {
    return S->getStmtClass() >= firstFTExecutableDirectiveConstant &&
           S->getStmtClass() <= lastFTExecutableDirectiveConstant;
  }

  child_range children() {
    if (!Data)
      return child_range(child_iterator(), child_iterator());
    return Data->getAssociatedStmtAsRange();
  }

  const_child_range children() const {
    return const_cast<FTExecutableDirective *>(this)->children();
  }

  ArrayRef<FTClause *> clauses() const {
    if (!Data)
      return std::nullopt;
    return Data->getClauses();
  }

  /// Returns whether or not this is a Standalone directive.
  ///
  /// Stand-alone directives are executable directives
  /// that have no associated user code.
  bool isStandaloneDirective() const;

  /// Returns the AST node representing FT structured-block of this
  /// FT executable directive,
  /// Prerequisite: Executable Directive must not be Standalone directive.
  const Stmt *getStructuredBlock() const {
    return const_cast<FTExecutableDirective *>(this)->getStructuredBlock();
  }
  Stmt *getStructuredBlock();

  const Stmt *getRawStmt() const {
    return const_cast<FTExecutableDirective *>(this)->getRawStmt();
  }
  Stmt *getRawStmt() {
    assert(hasAssociatedStmt() &&
           "Expected directive with the associated statement.");
    return Data->getRawStmt();
  }
};

class FTNmrDirective : public FTExecutableDirective {
  friend class ASTStmtReader;
  friend class FTExecutableDirective;
  /// true if the construct has inner cancel directive.
  bool HasCancel = false;

  /// Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive (directive keyword).
  /// \param EndLoc Ending Location of the directive.
  ///
  FTNmrDirective(SourceLocation StartLoc, SourceLocation EndLoc)
      : FTExecutableDirective(FTNmrDirectiveClass,
                               llvm::ft::FTD_nmr, StartLoc, EndLoc) {}

  /// Build an empty directive.
  ///
  explicit FTNmrDirective()
      : FTExecutableDirective(FTNmrDirectiveClass,
                               llvm::ft::FTD_nmr, SourceLocation(),
                               SourceLocation()) {}

  /// Sets special task reduction descriptor.
  void setTaskReductionRefExpr(Expr *E) { Data->getChildren()[0] = E; }

  /// Set cancel state.
  void setHasCancel(bool Has) { HasCancel = Has; }

public:
  /// Creates directive with a list of \a Clauses.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param Clauses List of clauses.
  /// \param AssociatedStmt Statement associated with the directive.
  /// \param TaskRedRef Task reduction special reference expression to handle
  /// taskgroup descriptor.
  /// \param HasCancel true if this directive has inner cancel directive.
  ///
  static FTNmrDirective *
  Create(const ASTContext &C, SourceLocation StartLoc, SourceLocation EndLoc,
         ArrayRef<FTClause *> Clauses, Stmt *AssociatedStmt);

  /// Creates an empty directive with the place for \a N clauses.
  ///
  /// \param C AST context.
  /// \param NumClauses Number of clauses.
  ///
  static FTNmrDirective *CreateEmpty(const ASTContext &C,
                                           unsigned NumClauses, EmptyShell);
  /// Returns special task reduction reference expression.
  Expr *getTaskReductionRefExpr() {
    return cast_or_null<Expr>(Data->getChildren()[0]);
  }
  const Expr *getTaskReductionRefExpr() const {
    return const_cast<FTNmrDirective *>(this)->getTaskReductionRefExpr();
  }

  /// Return true if current directive has inner cancel directive.
  bool hasCancel() const { return HasCancel; }
  static bool classof(const Stmt *T) {
    return T->getStmtClass() == FTNmrDirectiveClass;
  }
};

class FTVoteDirective : public FTExecutableDirective {
  friend class ASTStmtReader;
  friend class FTExecutableDirective;
  /// Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending location of the directive.
  ///
  FTVoteDirective(SourceLocation StartLoc, SourceLocation EndLoc)
      : FTExecutableDirective(FTVoteDirectiveClass, llvm::ft::FTD_vote,
                               StartLoc, EndLoc) {}

  /// Build an empty directive.
  ///
  explicit FTVoteDirective()
      : FTExecutableDirective(FTVoteDirectiveClass, llvm::ft::FTD_vote,
                               SourceLocation(), SourceLocation()) {}

public:
  /// Creates directive with a list of \a Clauses.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param Clauses List of clauses (only single FTVoteClause clause is
  /// allowed).
  ///
  static FTVoteDirective *Create(const ASTContext &C, SourceLocation StartLoc,
                                   SourceLocation EndLoc,
                                   ArrayRef<FTClause *> Clauses);

  /// Creates an empty directive with the place for \a NumClauses
  /// clauses.
  ///
  /// \param C AST context.
  /// \param NumClauses Number of clauses.
  ///
  static FTVoteDirective *CreateEmpty(const ASTContext &C,
                                        unsigned NumClauses, EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == FTVoteDirectiveClass;
  }
};


} // end namespace clang

#endif
