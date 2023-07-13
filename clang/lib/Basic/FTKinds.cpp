//===--- FTKinds.cpp - Token Kinds Support ----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file implements the FT enum and support functions.
///
//===----------------------------------------------------------------------===//

#include "clang/Basic/FTKinds.h"
#include "clang/Basic/IdentifierTable.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/ErrorHandling.h"
#include <cassert>

using namespace clang;
using namespace llvm::ft;
void clang::getFTCaptureRegions(
    SmallVectorImpl<FTDirectiveKind> &CaptureRegions,
    FTDirectiveKind DKind) {
  assert(unsigned(DKind) < llvm::ft::Directive_enumSize);
  switch (DKind) {
//  case FTD_vote:
  case FTD_nmr:
    CaptureRegions.push_back(FTD_nmr);
    break;
  default:
    llvm_unreachable("Unknown FT directive");
  }
}
