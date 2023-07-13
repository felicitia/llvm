//===- FTClause.cpp - Classes for FT clauses ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the subclesses of Stmt class declared in FTClause.h
//
//===----------------------------------------------------------------------===//

#include "clang/AST/FTClause.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclFT.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/FTKinds.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include <algorithm>
#include <cassert>

using namespace clang;
using namespace llvm;
using namespace ft;

FTClause::child_range FTClause::children() {
  switch (getClauseKind()) {
  default:
    break;
#define GEN_CLANG_CLAUSE_CLASS
#define CLAUSE_CLASS(Enum, Str, Class)                                         \
  case Enum:                                                                   \
    return static_cast<Class *>(this)->children();
#include "llvm/Frontend/FT/FT.inc"
  }
  llvm_unreachable("unknown FTClause");
}

FTClause::child_range FTClause::used_children() {
  switch (getClauseKind()) {
#define GEN_CLANG_CLAUSE_CLASS
#define CLAUSE_CLASS(Enum, Str, Class)                                         \
  case Enum:                                                                   \
    return static_cast<Class *>(this)->used_children();
#include "llvm/Frontend/FT/FT.inc"
  }
  llvm_unreachable("unknown FTClause");
}

FTVoteClause *FTVoteClause::Create(const ASTContext &C,
                                         SourceLocation StartLoc,
                                         SourceLocation LParenLoc,
                                         SourceLocation EndLoc,
                                         ArrayRef<Expr *> VL,
                                         ArrayRef<Expr *> SL,
                                         ArrayRef<Expr *> PL
					 ) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(VL.size()*3));
  FTVoteClause *Clause =
      new (Mem) FTVoteClause(StartLoc, LParenLoc, EndLoc, VL.size()*3);
  Clause->setVarSizePtrRefs(VL,SL,PL);
  return Clause;
}

FTVoteClause *FTVoteClause::CreateEmpty(const ASTContext &C, unsigned N) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(N*3));
  return new (Mem) FTVoteClause(N*3);
}

FTRhsClause *FTRhsClause::Create(const ASTContext &C,
                                         SourceLocation StartLoc,
                                         SourceLocation LParenLoc,
                                         SourceLocation EndLoc,
                                         ArrayRef<Expr *> VL,
                                         ArrayRef<Expr *> SL,
                                         ArrayRef<Expr *> PL) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(VL.size()*3));
  FTRhsClause *Clause =
      new (Mem) FTRhsClause(StartLoc, LParenLoc, EndLoc, VL.size()*3);
  Clause->setVarSizePtrRefs(VL, SL,PL);
  return Clause;
}

FTRhsClause *FTRhsClause::CreateEmpty(const ASTContext &C, unsigned N) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(N*3));
  return new (Mem) FTRhsClause(N*3);
}

FTLhsClause *FTLhsClause::Create(const ASTContext &C,
                                         SourceLocation StartLoc,
                                         SourceLocation LParenLoc,
                                         SourceLocation EndLoc,
                                         ArrayRef<Expr *> VL,
                                         ArrayRef<Expr *> SL,
                                         ArrayRef<Expr *> PL) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(VL.size()*3));
  FTLhsClause *Clause =
      new (Mem) FTLhsClause(StartLoc, LParenLoc, EndLoc, VL.size()*3);
  Clause->setVarSizePtrRefs(VL, SL,PL);
  return Clause;
}

FTLhsClause *FTLhsClause::CreateEmpty(const ASTContext &C, unsigned N) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(N*3));
  return new (Mem) FTLhsClause(N*3);
}

FTNovoteClause *FTNovoteClause::Create(const ASTContext &C,
                                         SourceLocation StartLoc,
                                         SourceLocation LParenLoc,
                                         SourceLocation EndLoc,
                                         ArrayRef<Expr *> VL,
                                         ArrayRef<Expr *> SL,
                                         ArrayRef<Expr *> PL) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(VL.size()*3));
  FTNovoteClause *Clause =
      new (Mem) FTNovoteClause(StartLoc, LParenLoc, EndLoc, VL.size()*3);
  Clause->setVarSizePtrRefs(VL,SL,PL);
  return Clause;
}

FTNovoteClause *FTNovoteClause::CreateEmpty(const ASTContext &C, unsigned N) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(N*3));
  return new (Mem) FTNovoteClause(N*3);
}

FTNorhsClause *FTNorhsClause::Create(const ASTContext &C,
                                         SourceLocation StartLoc,
                                         SourceLocation LParenLoc,
                                         SourceLocation EndLoc,
                                         ArrayRef<Expr *> VL,
                                         ArrayRef<Expr *> SL,
                                         ArrayRef<Expr *> PL) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(VL.size()*3));
  FTNorhsClause *Clause =
      new (Mem) FTNorhsClause(StartLoc, LParenLoc, EndLoc, VL.size()*3);
  Clause->setVarSizePtrRefs(VL, SL,PL);
  return Clause;
}

FTNorhsClause *FTNorhsClause::CreateEmpty(const ASTContext &C, unsigned N) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(N*3));
  return new (Mem) FTNorhsClause(N*3);
}

FTNolhsClause *FTNolhsClause::Create(const ASTContext &C,
                                         SourceLocation StartLoc,
                                         SourceLocation LParenLoc,
                                         SourceLocation EndLoc,
                                         ArrayRef<Expr *> VL,
                                         ArrayRef<Expr *> SL,
                                         ArrayRef<Expr *> PL) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(VL.size()*3));
  FTNolhsClause *Clause =
      new (Mem) FTNolhsClause(StartLoc, LParenLoc, EndLoc, VL.size()*3);
  Clause->setVarSizePtrRefs(VL, SL,PL);
  return Clause;
}

FTNolhsClause *FTNolhsClause::CreateEmpty(const ASTContext &C, unsigned N) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(N*3));
  return new (Mem) FTNolhsClause(N*3);
}

FTAutoClause *FTAutoClause::Create(const ASTContext &C,
                                         SourceLocation StartLoc,
                                         SourceLocation LParenLoc,
                                         SourceLocation EndLoc,
                                         ArrayRef<Expr *> VL,
                                         ArrayRef<Expr *> SL,
                                         ArrayRef<Expr *> PL) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(VL.size()*3));
  FTAutoClause *Clause =
      new (Mem) FTAutoClause(StartLoc, LParenLoc, EndLoc, VL.size()*3);
  Clause->setVarSizePtrRefs(VL, SL,PL);
  return Clause;
}

FTAutoClause *FTAutoClause::CreateEmpty(const ASTContext &C, unsigned N) {
  void *Mem = C.Allocate(totalSizeToAlloc<Expr *>(N*3));
  return new (Mem) FTAutoClause(N*3);
}
// endif

template<typename T>
void FTClausePrinter::VisitFTClauseList(T *Node, char StartSym) {
  for (typename T::varlist_iterator I = Node->varlist_begin(),
                                    E = Node->varlist_end();
       I != E; ++I) {
    assert(*I && "Expected non-null Stmt");
    OS << (I == Node->varlist_begin() ? StartSym : ',');
    if (auto *DRE = dyn_cast<DeclRefExpr>(*I)) {
/*      if (isa<FTCapturedExprDecl>(DRE->getDecl()))
        DRE->printPretty(OS, nullptr, Policy, 0);
      else */
        DRE->getDecl()->printQualifiedName(OS);
    } else
      (*I)->printPretty(OS, nullptr, Policy, 0);
  }
}

void FTClausePrinter::VisitFTVoteClause(FTVoteClause *Node) {
  if (!Node->varlist_empty()) {
    OS << "vote";
    VisitFTClauseList(Node, '(');
    OS << ")";
  }
}
void FTClausePrinter::VisitFTLhsClause(FTLhsClause *Node) {
  if (!Node->varlist_empty()) {
    OS << "lvar";
    VisitFTClauseList(Node, '(');
    OS << ")";
  }
}
void FTClausePrinter::VisitFTRhsClause(FTRhsClause *Node) {
  if (!Node->varlist_empty()) {
    OS << "rvar";
    VisitFTClauseList(Node, '(');
    OS << ")";
  }
}
void FTClausePrinter::VisitFTNovoteClause(FTNovoteClause *Node) {
  if (!Node->varlist_empty()) {
    OS << "novote";
    VisitFTClauseList(Node, '(');
    OS << ")";
  }
}
void FTClausePrinter::VisitFTNolhsClause(FTNolhsClause *Node) {
  if (!Node->varlist_empty()) {
    OS << "novar";
    VisitFTClauseList(Node, '(');
    OS << ")";
  }
}
void FTClausePrinter::VisitFTNorhsClause(FTNorhsClause *Node) {
  if (!Node->varlist_empty()) {
    OS << "norvar";
    VisitFTClauseList(Node, '(');
    OS << ")";
  }
}
void FTClausePrinter::VisitFTAutoClause(FTAutoClause *Node) {
  OS << "auto(";
    VisitFTClauseList(Node, '(');
  OS << ")";
}
