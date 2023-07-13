//===- DeclFT.h - Classes for representing FT directives -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file defines FT nodes for declarative directives.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_DECLFT_H
#define LLVM_CLANG_AST_DECLFT_H

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExternalASTSource.h"
#include "clang/AST/FTClause.h"
#include "clang/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/TrailingObjects.h"

namespace clang {
template <typename U> class FTDeclarativeDirective : public U {
  friend class ASTDeclReader;
  friend class ASTDeclWriter;

  /// Get the clauses storage.
  MutableArrayRef<FTClause *> getClauses() {
    if (!Data)
      return std::nullopt;
    return Data->getClauses();
  }

protected:
  /// Data, associated with the directive.
  FTChildren *Data = nullptr;

  /// Build instance of directive.
  template <typename... Params>
  FTDeclarativeDirective(Params &&... P) : U(std::forward<Params>(P)...) {}

  template <typename T, typename... Params>
  static T *createDirective(const ASTContext &C, DeclContext *DC,
                            ArrayRef<FTClause *> Clauses, unsigned NumChildren,
                            Params &&... P) {
    auto *Inst = new (C, DC, size(Clauses.size(), NumChildren))
        T(DC, std::forward<Params>(P)...);
    Inst->Data = FTChildren::Create(Inst + 1, Clauses,
                                     /*AssociatedStmt=*/nullptr, NumChildren);
    Inst->Data->setClauses(Clauses);
    return Inst;
  }

  template <typename T, typename... Params>
  static T *createEmptyDirective(const ASTContext &C, unsigned ID,
                                 unsigned NumClauses, unsigned NumChildren,
                                 Params &&... P) {
    auto *Inst = new (C, ID, size(NumClauses, NumChildren))
        T(nullptr, std::forward<Params>(P)...);
    Inst->Data = FTChildren::CreateEmpty(
        Inst + 1, NumClauses, /*HasAssociatedStmt=*/false, NumChildren);
    return Inst;
  }

  static size_t size(unsigned NumClauses, unsigned NumChildren) {
    return FTChildren::size(NumClauses, /*HasAssociatedStmt=*/false,
                             NumChildren);
  }

public:
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

  ArrayRef<FTClause *> clauses() const {
    if (!Data)
      return std::nullopt;
    return Data->getClauses();
  }
};

} // end namespace clang
#endif
