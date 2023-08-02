//===----- CGFTRuntime.cpp - Interface to FT Runtimes -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This provides a class for FT runtime code generation.
//
//===----------------------------------------------------------------------===//

#include "CGFTRuntime.h"
#include "CGCXXABI.h"
#include "CGCleanup.h"
#include "CGRecordLayout.h"
#include "CodeGenFunction.h"
#include "TargetInfo.h"
#include "clang/AST/APValue.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/FTClause.h"
#include "clang/AST/StmtFT.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Basic/BitmaskEnum.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/FTKinds.h"
#include "clang/Basic/SourceManager.h"
#include "clang/CodeGen/ConstantInitBuilder.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/AtomicOrdering.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <numeric>
#include <optional>

using namespace clang;
using namespace CodeGen;
using namespace llvm::ft;

CGFTRuntime::CGFTRuntime(CodeGenModule &CGM)
    : CGM(CGM), FTBuilder(CGM.getModule()) { 
  FTBuilder.initialize(StringRef{});
}

static llvm::ft::RuntimeFunction getVoteFnID(int code, bool isDebug) {
  auto FnID = (isDebug ? FTRTL___ft_vote_debug : FTRTL___ft_vote);
  if (code & 0x8) {
    switch(code & (0x2 | 0x8)) {
      case 8: FnID = (isDebug ? FTRTL___ft_votenow_debug : FTRTL___ft_votenow); break;
      case 10: FnID = (isDebug ? FTRTL___ft_atomic_votenow_debug : FTRTL___ft_atomic_votenow); break;
    }
  } else {
    switch(code & (0x1 | 0x2 | 0x4)) {
      case 0: FnID = (isDebug ? FTRTL___ft_votel_debug : FTRTL___ft_votel); break;
      case 1: FnID = (isDebug ? FTRTL___ft_voter_debug : FTRTL___ft_voter); break;
      case 2: FnID = (isDebug ? FTRTL___ft_atomic_votel_debug : FTRTL___ft_atomic_votel); break;
      case 3: FnID = (isDebug ? FTRTL___ft_atomic_voter_debug : FTRTL___ft_atomic_voter); break;
      case 4: FnID = (isDebug ? FTRTL___ft_auto_votel_debug : FTRTL___ft_auto_votel); break;
      case 5: FnID = (isDebug ? FTRTL___ft_auto_voter_debug : FTRTL___ft_auto_voter); break;
      case 6: FnID = (isDebug ? FTRTL___ft_auto_atomic_votel_debug: FTRTL___ft_auto_atomic_votel); break;
      case 7: FnID = (isDebug ? FTRTL___ft_auto_atomic_voter_debug : FTRTL___ft_auto_atomic_voter); break;
    }
  }
  return FnID;
}

void CGFTRuntime::emitVoteCall(CodeGenFunction &CGF, llvm::Value * AddrPtr, uint64_t sizeOfType, int whichSide) {

  if (!CGF.HaveInsertPoint())
    return;
  llvm::Value *Args[] = {
    AddrPtr,
    CGF.Builder.CreateIntCast(llvm::ConstantInt::get(CGM.Int32Ty, sizeOfType), CGM.Int32Ty, /*isSigned*/ true),
  };
  auto FnID = FTRTL___ft_vote;
  if (whichSide & 0x8) {
    switch(whichSide & (0x2 | 0x8)) {
      case 8: FnID = FTRTL___ft_votenow; break;
      case 10: FnID = FTRTL___ft_atomic_votenow; break;
    }
  } else {
    switch(whichSide & (0x1 | 0x2 | 0x4)) {
      case 0: FnID = FTRTL___ft_votel; break;
      case 1: FnID = FTRTL___ft_voter; break;
      case 2: FnID = FTRTL___ft_atomic_votel; break;
      case 3: FnID = FTRTL___ft_atomic_voter; break;
      case 4: FnID = FTRTL___ft_auto_votel; break;
      case 5: FnID = FTRTL___ft_auto_voter; break;
      case 6: FnID = FTRTL___ft_auto_atomic_votel; break;
      case 7: FnID = FTRTL___ft_auto_atomic_voter; break;
    }
  }
  CGF.EmitRuntimeCall(FTBuilder.getOrCreateRuntimeFunction(
                         CGM.getModule(), FnID),
                     Args);
}

void CGFTRuntime::emitVoteCallDebug(CodeGenFunction &CGF, llvm::Value * AddrPtr, uint64_t sizeOfType, llvm::Value * StrPtr, int lineNo , int whichSide) {

  if (!CGF.HaveInsertPoint())
    return;
  llvm::Value *Args[] = {
    AddrPtr,
    CGF.Builder.CreateIntCast(llvm::ConstantInt::get(CGM.Int32Ty, sizeOfType), CGM.Int32Ty, /*isSigned*/ true),
    StrPtr,
    CGF.Builder.CreateIntCast(llvm::ConstantInt::get(CGM.Int32Ty, lineNo), CGM.Int32Ty, /*isSigned*/ true)
  };
  auto FnID = FTRTL___ft_vote;
  if (whichSide & 0x8) {
    switch(whichSide & (0x2 | 0x8)) {
      case 8: FnID = FTRTL___ft_votenow_debug; break;
      case 10: FnID = FTRTL___ft_atomic_votenow_debug; break;
    }
  } else {
    switch(whichSide & (0x1 | 0x2 | 0x4)) {
      case 0: FnID = FTRTL___ft_votel_debug; break;
      case 1: FnID = FTRTL___ft_voter_debug; break;
      case 2: FnID = FTRTL___ft_atomic_votel_debug; break;
      case 3: FnID = FTRTL___ft_atomic_voter_debug; break;
      case 4: FnID = FTRTL___ft_auto_votel_debug; break;
      case 5: FnID = FTRTL___ft_auto_voter_debug; break;
      case 6: FnID = FTRTL___ft_auto_atomic_votel_debug; break;
      case 7: FnID = FTRTL___ft_auto_atomic_voter_debug; break;
    }
  }
  CGF.EmitRuntimeCall(FTBuilder.getOrCreateRuntimeFunction(
                         CGM.getModule(), FnID),
                     Args);
}
