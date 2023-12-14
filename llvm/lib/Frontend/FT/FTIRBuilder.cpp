//===- FTIRBuilder.cpp - Builder for LLVM-IR for FT directives ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
///
/// This file implements the FTIRBuilder class, which is used as a
/// convenient way to create LLVM instructions for FT directives.
///
//===----------------------------------------------------------------------===//

#include "llvm/Frontend/FT/FTIRBuilder.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/CodeMetrics.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Value.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/CodeExtractor.h"
#include "llvm/Transforms/Utils/LoopPeel.h"
#include "llvm/Transforms/Utils/UnrollLoop.h"

#include <cstdint>
#include <optional>

#define DEBUG_TYPE "ft-ir-builder"

using namespace llvm;
using namespace ft;

#if 1
void FTIRBuilder::initializeTypes(Module &M) {
  LLVMContext &Ctx = M.getContext();
//  StructType *T;
#define FT_TYPE(VarName, InitValue) VarName = InitValue;
#define FT_FUNCTION_TYPE(VarName, IsVarArg, ReturnType, ...)                  \
  VarName = FunctionType::get(ReturnType, {__VA_ARGS__}, IsVarArg);            \
  VarName##Ptr = PointerType::getUnqual(VarName);
#include "llvm/Frontend/FT/FTKinds.def"
}

void FTIRBuilder::initialize(StringRef HostFilePath) {
  initializeTypes(M);

  if (HostFilePath.empty())
    return;

  auto Buf = MemoryBuffer::getFile(HostFilePath);
  if (std::error_code Err = Buf.getError()) {
    report_fatal_error(("error opening host file from host file path inside of "
                        "FTIRBuilder: " +
                        Err.message())
                           .c_str());
  }

  LLVMContext Ctx;
  auto M = expectedToErrorOrAndEmitErrors(
      Ctx, parseBitcodeFile(Buf.get()->getMemBufferRef(), Ctx));
  if (std::error_code Err = M.getError()) {
    report_fatal_error(
        ("error parsing host file inside of FTIRBuilder: " + Err.message())
            .c_str());
  }

  loadOffloadInfoMetadata(*M.get());
}

void FTIRBuilder::loadOffloadInfoMetadata(Module &M) {
  // If we are in target mode, load the metadata from the host IR. This code has
  // to match the metadata creation in createOffloadEntriesAndInfoMetadata().

  NamedMDNode *MD = M.getNamedMetadata(ftOffloadInfoName);
  if (!MD)
    return;

  for (MDNode *MN : MD->operands()) {
    auto &&GetMDInt = [MN](unsigned Idx) {
      auto *V = cast<ConstantAsMetadata>(MN->getOperand(Idx));
      return cast<ConstantInt>(V->getValue())->getZExtValue();
    };

    auto &&GetMDString = [MN](unsigned Idx) {
      auto *V = cast<MDString>(MN->getOperand(Idx));
      return V->getString();
    };

    switch (GetMDInt(0)) {
    default:
      llvm_unreachable("Unexpected metadata!");
      break;
#if 0
    case OffloadEntriesInfoManager::OffloadEntryInfo::
        OffloadingEntryInfoTargetRegion: {
      TargetRegionEntryInfo EntryInfo(/*ParentName=*/GetMDString(3),
                                      /*DeviceID=*/GetMDInt(1),
                                      /*FileID=*/GetMDInt(2),
                                      /*Line=*/GetMDInt(4),
                                      /*Count=*/GetMDInt(5));
      OffloadInfoManager.initializeTargetRegionEntryInfo(EntryInfo,
                                                         /*Order=*/GetMDInt(6));
      break;
    }
    case OffloadEntriesInfoManager::OffloadEntryInfo::
        OffloadingEntryInfoDeviceGlobalVar:
      OffloadInfoManager.initializeDeviceGlobalVarEntryInfo(
          /*MangledName=*/GetMDString(1),
          static_cast<OffloadEntriesInfoManager::OMPTargetGlobalVarEntryKind>(
              /*Flags=*/GetMDInt(2)),
          /*Order=*/GetMDInt(3));
      break;
#endif
    }
  }
}
#endif

FunctionCallee
FTIRBuilder::getOrCreateRuntimeFunction(Module &M, RuntimeFunction FnID) {
  FunctionType *FnTy = nullptr;
  Function *Fn = nullptr;

  // Try to find the declation in the module first.
  switch (FnID) {
#define FT_RTL(Enum, Str, IsVarArg, ReturnType, ...)                          \
  case Enum:                                                                   \
    FnTy = FunctionType::get(ReturnType, ArrayRef<Type *>{__VA_ARGS__},        \
                             IsVarArg);                                        \
    Fn = M.getFunction(Str);                                                   \
    break;
#include "llvm/Frontend/FT/FTKinds.def"
  }

  if (!Fn) {
    // Create a new declaration if we need one.
    switch (FnID) {
#define FT_RTL(Enum, Str, ...)                                                \
  case Enum:                                                                   \
    Fn = Function::Create(FnTy, GlobalValue::ExternalLinkage, Str, M);         \
    break;
#include "llvm/Frontend/FT/FTKinds.def"
    }

    LLVM_DEBUG(dbgs() << "Created FT runtime function " << Fn->getName()
                      << " with type " << *Fn->getFunctionType() << "\n");
    // addAttributes(FnID, *Fn);

  } else {
    LLVM_DEBUG(dbgs() << "Found FT runtime function " << Fn->getName()
                      << " with type " << *Fn->getFunctionType() << "\n");
  }

  assert(Fn && "Failed to create FT runtime function");

  return {FnTy, Fn};
}

