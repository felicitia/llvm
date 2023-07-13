//===--- FTKinds.h - FT enums ---------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines some FT-specific enums and functions.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_BASIC_FTKINDS_H
#define LLVM_CLANG_BASIC_FTKINDS_H

#include "clang/Basic/LangOptions.h"
#include "llvm/ADT/StringRef.h"
//#include "llvm/Frontend/FT/OMPConstants.h"
#include "llvm/Frontend/FT/FTConstants.h"

namespace clang {

/// FT directives.
using FTDirectiveKind = llvm::ft::Directive;

/// FT clauses.
using FTClauseKind = llvm::ft::Clause;

/// Return the captured regions of an OpenMP directive.
void getFTCaptureRegions(
    llvm::SmallVectorImpl<FTDirectiveKind> &CaptureRegions,
    FTDirectiveKind DKind);
}
#endif
