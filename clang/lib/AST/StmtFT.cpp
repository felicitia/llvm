//===--- StmtFT.cpp - Classes for FT directives -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the subclesses of Stmt class declared in StmtFT.h
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/StmtFT.h"

using namespace clang;
using namespace llvm::ft;

size_t FTChildren::size(unsigned NumClauses, bool HasAssociatedStmt,
                         unsigned NumChildren) {
  return llvm::alignTo(
      totalSizeToAlloc<FTClause *, Stmt *>(
          NumClauses, NumChildren + (HasAssociatedStmt ? 1 : 0)),
      alignof(FTChildren));
}

void FTChildren::setClauses(ArrayRef<FTClause *> Clauses) {
  assert(Clauses.size() == NumClauses &&
         "Number of clauses is not the same as the preallocated buffer");
  llvm::copy(Clauses, getTrailingObjects<FTClause *>());
}

MutableArrayRef<Stmt *> FTChildren::getChildren() {
  return llvm::makeMutableArrayRef(getTrailingObjects<Stmt *>(), NumChildren);
}

FTChildren *FTChildren::Create(void *Mem, ArrayRef<FTClause *> Clauses) {
  auto *Data = CreateEmpty(Mem, Clauses.size());
  Data->setClauses(Clauses);
  return Data;
}

FTChildren *FTChildren::Create(void *Mem, ArrayRef<FTClause *> Clauses,
                                 Stmt *S, unsigned NumChildren) {
  auto *Data = CreateEmpty(Mem, Clauses.size(), S, NumChildren);
  Data->setClauses(Clauses);
  if (S)
    Data->setAssociatedStmt(S);
  return Data;
}

FTChildren *FTChildren::CreateEmpty(void *Mem, unsigned NumClauses,
                                      bool HasAssociatedStmt,
                                      unsigned NumChildren) {
  return new (Mem) FTChildren(NumClauses, NumChildren, HasAssociatedStmt);
}

FTVoteDirective *FTVoteDirective::Create(const ASTContext &C,
                                             SourceLocation StartLoc,
                                             SourceLocation EndLoc,
                                             ArrayRef<FTClause *> Clauses) {
  return createDirective<FTVoteDirective>(
      C, Clauses, /*AssociatedStmt=*/nullptr, /*NumChildren=*/0, StartLoc,
      EndLoc);
}

bool FTExecutableDirective::isStandaloneDirective() const {
  // Special case: 'omp target enter data', 'omp target exit data',
  // 'omp target update' are stand-alone directives, but for implementation
  // reasons they have empty synthetic structured block, to simplify codegen.
  return !hasAssociatedStmt();
}

Stmt *FTExecutableDirective::getStructuredBlock() {
  assert(!isStandaloneDirective() &&
         "Standalone Executable Directives don't have Structured Blocks.");
  return getRawStmt();
}

FTVoteDirective *FTVoteDirective::CreateEmpty(const ASTContext &C,
                                                  unsigned NumClauses,
                                                  EmptyShell) {
  return createEmptyDirective<FTVoteDirective>(C, NumClauses);
}

FTNmrDirective *FTNmrDirective::Create(
    const ASTContext &C, SourceLocation StartLoc, SourceLocation EndLoc,
    ArrayRef<FTClause *> Clauses, Stmt *AssociatedStmt) {
  auto *Dir = createDirective<FTNmrDirective>(
      C, Clauses, AssociatedStmt, /*NumChildren=*/1, StartLoc, EndLoc);
//  Dir->setTaskReductionRefExpr(TaskRedRef);
//  Dir->setHasCancel(HasCancel);
  return Dir;
}

FTNmrDirective *FTNmrDirective::CreateEmpty(const ASTContext &C,
                                                        unsigned NumClauses,
                                                        EmptyShell) {
  return createEmptyDirective<FTNmrDirective>(C, NumClauses,
                                                    /*HasAssociatedStmt=*/true,
                                                    /*NumChildren=*/1);
}

