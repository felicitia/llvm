//===-- FT.td - FT directive definition file ----------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This is the definition file for FT directives and clauses.
//
//===----------------------------------------------------------------------===//

include "llvm/Frontend/Directive/DirectiveBase.td"

//===----------------------------------------------------------------------===//
// Definition of general FT information
//===----------------------------------------------------------------------===//

def FT : DirectiveLanguage {
  let name = "FT";
  let cppNamespace = "ft"; // final namespace will be llvm::ft
  let directivePrefix = "FTD_";
  let clausePrefix = "FTC_";
  let makeEnumAvailableInNamespace = true;
  let enableBitmaskEnumInNamespace = true;
  let clauseEnumSetClass = "FtClauseSet";
  let flangClauseBaseClass = "FtClause";
}

def FTC_Lhs: Clause<"lhs"> {	/* DK: TODO */
  let clangClass = "FTLhsClause";
  let flangClass = "FtObjectList";
}
def FTC_Rhs: Clause<"rhs"> {	/* DK: TODO */
  let clangClass = "FTRhsClause";
  let flangClass = "FtObjectList";
}
def FTC_Nolhs: Clause<"nolhs"> {	/* DK: TODO */
  let clangClass = "FTNolhsClause";
  let flangClass = "FtObjectList";
}
def FTC_Norhs: Clause<"norhs"> {	/* DK: TODO */
  let clangClass = "FTNorhsClause";
  let flangClass = "FtObjectList";
}
def FTC_Novote: Clause<"novote"> {	/* DK: TODO */
  let clangClass = "FTNovoteClause";
  let flangClass = "FtObjectList";
}
def FTC_Auto: Clause<"auto"> {	/* DK: TODO */
  let clangClass = "FTAutoClause";
  let flangClass = "FtObjectList";
}
def FTC_Unknown : Clause<"unknown"> {
  let isImplicit = true;
  let isDefault = true;
}
def FTC_Vote: Clause<"vote"> {
  let clangClass = "FTVoteClause";
  let isImplicit = true;
}

//===----------------------------------------------------------------------===//
// Definition of FT directives
//===----------------------------------------------------------------------===//

def FT_Nmr : Directive<"nmr"> {
  let allowedClauses = [
    VersionedClause<FTC_Lhs>,
    VersionedClause<FTC_Vote>,
    VersionedClause<FTC_Rhs>,
    VersionedClause<FTC_Nolhs>,
    VersionedClause<FTC_Norhs>,
    VersionedClause<FTC_Novote>,
    VersionedClause<FTC_Auto>,
  ];
}
def FT_Vote : Directive<"vote"> {
  let allowedClauses = [
    VersionedClause<FTC_Vote>,
  ];
}
def FT_Unknown : Directive<"unknown"> {
  let isDefault = true;
}
// endif
