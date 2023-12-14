//===--- ParseFT.cpp - FT directives parsing ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file implements parsing of all FT directives and clauses.
///
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/FTClause.h"
#include "clang/AST/StmtFT.h"
#include "clang/Basic/FTKinds.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TokenKinds.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/Sema/Scope.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/UniqueVector.h"
//#include "llvm/Frontend/FT/OMPAssume.h"
//#include "llvm/Frontend/FT/OMPContext.h"
#include "llvm/Frontend/FT/FTConstants.h"

using namespace clang;
using namespace llvm::ft;

#if 0
llvm::StringRef llvm::ft::getFTDirectiveName(Directive Kind) {
  switch (Kind) {
    case FTD_nmr:
      return "nmr";
    case FTD_unknown:
      return "unknown";
    case FTD_vote:
      return "vote";
  }
  llvm_unreachable("Invalid FT Directive kind");
}

Clause llvm::ft::getFTClauseKind(llvm::StringRef Str) {
  return llvm::StringSwitch<Clause>(Str)
    .Case("auto",FTC_auto)
    .Case("lhs",FTC_lhs)
    .Case("nolhs",FTC_nolhs)
    .Case("norhs",FTC_norhs)
    .Case("novote",FTC_novote)
    .Case("rhs",FTC_rhs)
    .Case("unknown",FTC_unknown)
    .Case("vote",FTC_unknown)
    .Default(FTC_unknown);
}

llvm::StringRef llvm::ft::getFTClauseName(Clause Kind) {
  switch (Kind) {
    case FTC_auto:
      return "auto";
    case FTC_lhs:
      return "lhs";
    case FTC_nolhs:
      return "nolhs";
    case FTC_norhs:
      return "norhs";
    case FTC_novote:
      return "novote";
    case FTC_rhs:
      return "rhs";
    case FTC_unknown:
      return "unknown";
    case FTC_vote:
      return "vote";
  }
  llvm_unreachable("Invalid FT Clause kind");
}
#endif
//===----------------------------------------------------------------------===//
// FT declarative directives.
//===----------------------------------------------------------------------===//

struct FTDirectiveKindExWrapper {
  FTDirectiveKindExWrapper(unsigned Value) : Value(Value) {}
  FTDirectiveKindExWrapper(FTDirectiveKind DK) : Value(unsigned(DK)) {}
  bool operator==(FTDirectiveKindExWrapper V) const {
    return Value == V.Value;
  }
  bool operator!=(FTDirectiveKindExWrapper V) const {
    return Value != V.Value;
  }
  bool operator==(FTDirectiveKind V) const { return Value == unsigned(V); }
  bool operator!=(FTDirectiveKind V) const { return Value != unsigned(V); }
  bool operator<(FTDirectiveKind V) const { return Value < unsigned(V); }
  operator unsigned() const { return Value; }
  operator FTDirectiveKind() const { return FTDirectiveKind(Value); }
  unsigned Value;
};
static bool withinOpenMPScope(Parser *Self) {
  Scope * PScope = Self->getCurScope();
  while (PScope) {
    if (PScope->isOpenMPDirectiveScope() || PScope->isOpenMPLoopDirectiveScope() || PScope->isOpenMPSimdDirectiveScope()) return true;
    PScope = PScope->getParent();
  }
  return false;
}

Parser::DeclGroupPtrTy Parser::ParseFTDeclarativeDirectiveWithExtDecl(
    AccessSpecifier &AS, ParsedAttributes &Attrs, bool Delayed,
    DeclSpec::TST TagType, Decl *Tag) {
  assert(Tok.isOneOf(tok::annot_pragma_ft, tok::annot_attr_ft) &&
         "Not an FT directive!");
  while (Tok.isNot(tok::annot_pragma_ft_end))
    ConsumeAnyToken();
  ConsumeAnyToken();
  return nullptr;
}

StmtResult Parser::ParseFTDeclarativeOrExecutableDirective(
    ParsedStmtContext StmtCtx, bool ReadDirectiveWithinMetadirective) {
  SmallVector<FTClause *, 5> Clauses;
  SmallVector<llvm::PointerIntPair<FTClause *, 1, bool>,
              llvm::omp::Clause_enumSize + 1>
      FirstClauses(llvm::omp::Clause_enumSize + 1);	// DK: track if the same clause occurs again
  StmtResult Directive = StmtError();
  DeclarationNameInfo DirName;
  bool HasAssociatedStatement = true;
  FTDirectiveKind DKind;
  Token ImplicitTok;
  bool ImplicitClauseAllowed = false;
  unsigned ScopeFlags = Scope::FnScope | Scope::DeclScope |
                        Scope::CompoundStmtScope | Scope::FTDirectiveScope;

  SourceLocation Loc = ReadDirectiveWithinMetadirective
                           ? Tok.getLocation()
                           : ConsumeAnnotationToken(),
                 EndLoc;
  Token _Tok = getCurToken();
  StringRef S = getPreprocessor().getSpelling(_Tok);
  DKind = llvm::StringSwitch<FTDirectiveKindExWrapper>(S)
	.Case("vote", FTD_vote)
	.Case("nmr", FTD_nmr)
        .Default(FTD_unknown);

  switch (DKind) {
  default:
    Diag(Tok, diag::err_ft_unexpected_directive)
        << 1 << getFTDirectiveName(DKind);
    SkipUntil(tok::annot_pragma_ft_end);
//    assert(false && "Not an FT directive!");
    break;
  case FTD_vote:
  case FTD_nmr:
    if (DKind == FTD_vote) {
      HasAssociatedStatement = false;
      ImplicitTok = Tok;
    }
    else if (DKind == FTD_nmr) {
      HasAssociatedStatement = true;
    } else {
        return StmtError();
    }
    if (withinOpenMPScope(this)) {
        // if FT is declared within an OpenMP region, reject it as error
        return StmtError();
    }
    ConsumeToken();
    ImplicitClauseAllowed = true;
    ParseScope FTDirectiveScope(this, ScopeFlags);
    Actions.StartFTDSABlock(DKind, DirName, Actions.getCurScope(), Loc);	// DK: FIX IT
    while (Tok.isNot(tok::annot_pragma_ft_end)) {
      // If we are parsing for a directive within a metadirective, the directive
      // ends with a ')'.
      if (ReadDirectiveWithinMetadirective && Tok.is(tok::r_paren)) {
        while (Tok.isNot(tok::annot_pragma_ft_end))
          ConsumeAnyToken();
        break;
      }
      bool HasImplicitClause = false;
      if (ImplicitClauseAllowed && Tok.is(tok::l_paren)) {
        HasImplicitClause = true;
        // Push copy of the current token back to stream to properly parse
        // pseudo-clause OMPFlushClause or OMPDepobjClause.
        PP.EnterToken(Tok, /*IsReinject*/ true);
        PP.EnterToken(ImplicitTok, /*IsReinject*/ true);
        ConsumeAnyToken();
      }
      FTClauseKind CKind = Tok.isAnnotation()
                                   ? FTC_unknown
                                   : getFTClauseKind(PP.getSpelling(Tok));
      if (HasImplicitClause) {
        assert(CKind == FTC_unknown && "Must be unknown implicit clause.");
      }
      if (DKind == FTD_vote)	
	      CKind = FTC_vote;

      // No more implicit clauses allowed.
      ImplicitClauseAllowed = false;
      Actions.StartFTClause(CKind);
      HasImplicitClause = false;
      FTClause *Clause = ParseFTClause(
          DKind, CKind, !FirstClauses[unsigned(CKind)].getInt());
      FirstClauses[unsigned(CKind)].setInt(true);
      if (Clause) {
        FirstClauses[unsigned(CKind)].setPointer(Clause);
        Clauses.push_back(Clause);
      }

      // Skip ',' if any.
      if (Tok.is(tok::comma))
        ConsumeToken();
      Actions.EndFTClause();
    }
    // End location of the directive.
    EndLoc = Tok.getLocation();
    // Consume final annot_pragma_openmp_end.
    ConsumeAnnotationToken();

    StmtResult AssociatedStmt;
    if (HasAssociatedStatement) {	// DK: region
      assert(DKind == FTD_nmr && "Only NMR directive has associated statements!");
//        ParsingOpenMPDirectiveRAII NormalScope(*this, /*Value=*/false);	// DK: without it, it looks OK.
        {
          Sema::CompoundScopeRAII Scope(Actions);
          AssociatedStmt = ParseStatement();	// DK: here associated statements are parsed
	}
    } 
    Directive = Actions.ActOnFTExecutableDirective(
        DKind, DirName, Clauses, AssociatedStmt.get(), Loc,
        EndLoc);

    // Exit scope.
    Actions.EndFTDSABlock(Directive.get());	// DK: FIX IT
    FTDirectiveScope.Exit();
    break;
  }
  return Directive;
}

static bool isAllowedFTClauseForDirective(FTDirectiveKind DKind, FTClauseKind CKind) {

  if (DKind == FTD_nmr) {
    switch (CKind) {
      case FTC_vote:
      case FTC_lhs:
      case FTC_rhs:
      case FTC_auto:
      case FTC_novote:
      case FTC_nolhs:
      case FTC_norhs:
	return true;
    }
  } else if (DKind == FTD_vote) {
    if (CKind == FTC_vote) {
      return true;
    }
  }
  return false;
}

FTClause *Parser::ParseFTDoubleVarListClause(FTDirectiveKind DKind,
                                            FTClauseKind Kind,
                                            bool ParseOnly) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LOpen = ConsumeToken();
  SmallVector<Expr *, 4> Vars;
  SmallVector<Expr *, 4> Sizes;
  SmallVector<Expr *, 4> Ptr;

  bool IsComma = true;
  BalancedDelimiterTracker T(*this, tok::l_paren, tok::annot_pragma_ft_end);
  if (T.expectAndConsume(diag::err_expected_lparen_after,
                         getFTClauseName(Kind).data()))
    return nullptr;

  while (Tok.isNot(tok::r_paren) /* && Tok.isNot(tok::colon) */ &&
                     Tok.isNot(tok::annot_pragma_ft_end)) {
    // Parse variable
    ExprResult VarExpr =
        Actions.CorrectDelayedTyposInExpr(ParseAssignmentExpression());
    if (VarExpr.isUsable()) {
      const Expr* expr = VarExpr.get()->IgnoreParenCasts();
      if (isa<DeclRefExpr>(expr)) 
        Vars.push_back(VarExpr.get());
      else
        Diag(Tok, diag::warn_ft_vote_no_simple_var) << getFTDirectiveName(DKind) << getFTClauseName(Kind);
   
    } else {
      SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_ft_end,
                StopBeforeMatch);
    }
    // Skip ',' if any
    bool IsColon = Tok.is(tok::colon);
    bool IsColonColon = Tok.is(tok::coloncolon);
    IsComma = Tok.is(tok::comma);
    SourceLocation ColonLoc; 
    if (IsColon) {  // size is specified explicitly
#if 0
      DataSizes.ColonLoc = ConsumeToken();	// DK: we may not need this. It is overwritten anyway
#else
      ColonLoc = ConsumeToken();
#endif
      // Parse variable
      SourceLocation ELoc = Tok.getLocation();
      ExprResult LHS(
          ParseCastExpression(AnyCastExpr, false, NotTypeCast));
      ExprResult Val(ParseRHSOfBinaryExpression(LHS, prec::Conditional));
      Val = Actions.ActOnFinishFullExpr(Val.get(), ELoc, /*DiscardedValue*/ false);
      // Parse variable
      if (Val.isInvalid()) {
        return nullptr;
      } else {
        Sizes.push_back(Val.get());
        SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_ft_end, tok::colon,
                  StopBeforeMatch);
      }
      IsComma = Tok.is(tok::comma);
      IsColon = Tok.is(tok::colon);
      if (IsColon) { // ptr depth field
#if 0
        DataSizes.ColonLoc = ConsumeToken();	// DK: we may not need this. It is overwritten anyway
#else
        ColonLoc = ConsumeToken();	// DK: we may not need this. It is overwritten anyway
#endif
        // Parse variable
        SourceLocation ELoc = Tok.getLocation();
        ExprResult LHS(
            ParseCastExpression(AnyCastExpr, false, NotTypeCast));
        ExprResult Val(ParseRHSOfBinaryExpression(LHS, prec::Conditional));
        Val = Actions.ActOnFinishFullExpr(Val.get(), ELoc, /*DiscardedValue*/ false);
        // Parse variable
        if (Val.isInvalid()) {
          return nullptr;
        } else {
          Ptr.push_back(Val.get());
          SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_ft_end,
                    StopBeforeMatch);
        }
        IsComma = Tok.is(tok::comma);
      } else
        Ptr.push_back(nullptr);
    }  else { // use default value or size of the variable
        Sizes.push_back(nullptr);
	if (IsColonColon) {
#if 0
          DataSizes.ColonLoc = ConsumeToken();	// DK: we may not need this. It is overwritten anyway
#else
          ColonLoc = ConsumeToken();	// DK: we may not need this. It is overwritten anyway
#endif
          // Parse variable
          SourceLocation ELoc = Tok.getLocation();
          ExprResult LHS(
              ParseCastExpression(AnyCastExpr, false, NotTypeCast));
          ExprResult Val(ParseRHSOfBinaryExpression(LHS, prec::Conditional));
          Val = Actions.ActOnFinishFullExpr(Val.get(), ELoc, /*DiscardedValue*/ false);
          // Parse variable
          if (Val.isInvalid()) {
            return nullptr;
          } else {
            Ptr.push_back(Val.get());
            SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_ft_end,
                      StopBeforeMatch);
          }
          IsComma = Tok.is(tok::comma);
	} else {
		Ptr.push_back(nullptr);
	}
    }
    if (IsComma)
      ConsumeToken();
  }

#if 0
  // Parse ')'.
  Data.RLoc = Tok.getLocation();
  if (!T.consumeClose())
    Data.RLoc = T.getCloseLocation();
#else
  SourceLocation RLoc = Tok.getLocation();
  if (!T.consumeClose())
    RLoc = T.getCloseLocation();
  
#endif
  if (ParseOnly)
    return nullptr;
#if 0
  OMPVarListLocTy Locs(Loc, LOpen, Data.RLoc);
  SourceLocation StartLoc = Locs.StartLoc;
  SourceLocation LParenLoc = Locs.LParenLoc;
  SourceLocation EndLoc = Locs.EndLoc;
#else
  SourceLocation StartLoc = Loc;
  SourceLocation LParenLoc = LOpen;
  SourceLocation EndLoc = RLoc;
#endif
  return Actions.ActOnFTVarSizeListClause(
      Kind, Vars, Sizes, Ptr, StartLoc, LParenLoc, EndLoc);
}

void Parser::skipUntilPragmaFTEnd(FTDirectiveKind DKind) {
  // The last seen token is annot_pragma_ft_end - need to check for
  // extra tokens.
  if (Tok.is(tok::annot_pragma_ft_end))
    return;

  Diag(Tok, diag::warn_omp_extra_tokens_at_eol)
      << getFTDirectiveName(DKind);
  while (Tok.isNot(tok::annot_pragma_ft_end))
    ConsumeAnyToken();
}

FTClause *Parser::ParseFTClause(FTDirectiveKind DKind,
                                     FTClauseKind CKind, bool FirstClause) {
  FTClause *Clause = nullptr;
  bool ErrorFound = false;
  bool WrongDirective = false;
  if (CKind == FTC_unknown ||
      !isAllowedFTClauseForDirective(DKind, CKind)) {
    Diag(Tok, diag::err_ft_unexpected_clause)
        << getFTClauseName(CKind) << getFTDirectiveName(DKind);
    ErrorFound = true;
    WrongDirective = true;
  }
  switch (CKind) {
    case FTC_vote:	/* vote now */
        Clause = ParseFTDoubleVarListClause(DKind, CKind, WrongDirective);
	break;
    case FTC_lhs:	/* LVALUE */
        Clause = ParseFTDoubleVarListClause(DKind, CKind, WrongDirective);
	break;
    case FTC_rhs:	/* RVALUE */
        Clause = ParseFTDoubleVarListClause(DKind, CKind, WrongDirective);
	break;
    case FTC_novote:	/* LVALUE */
        Clause = ParseFTDoubleVarListClause(DKind, CKind, WrongDirective);
	break;
    case FTC_nolhs:	/* LVALUE */
        Clause = ParseFTDoubleVarListClause(DKind, CKind, WrongDirective);
	break;
    case FTC_norhs:	/* RVALUE */
        Clause = ParseFTDoubleVarListClause(DKind, CKind, WrongDirective);
	break;
    case FTC_auto:	/* TODO: degree of NMR */
        Clause = ParseFTDoubleVarListClause(DKind, CKind, WrongDirective);
        break;
    case FTC_unknown:
        skipUntilPragmaFTEnd(DKind);
        break;
    default:
	  ErrorFound = true;
  }
  return ErrorFound ? nullptr : Clause;
}

