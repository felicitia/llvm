//===- IR/FTIRBuilder.h - FT encoding builder for LLVM IR - C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the FTIRBuilder class and helpers used as a convenient
// way to create LLVM instructions for FT directives.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FRONTEND_FT_FTIRBUILDER_H
#define LLVM_FRONTEND_FT_FTIRBUILDER_H

#include "llvm/Analysis/MemorySSAUpdater.h"
#include "llvm/Frontend/FT/FTConstants.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/Allocator.h"
#include <forward_list>
#include <map>
#include <optional>

namespace llvm {
class FTIRBuilder;

class FTIRBuilder {
public:
  /// Create a new FTIRBuilder operating on the given module \p M. This will
  /// not have an effect on \p M (see initialize)
  FTIRBuilder(Module &M)
      : M(M), Builder(M.getContext()) {}
  ~FTIRBuilder() {};
  FunctionCallee getOrCreateRuntimeFunction(Module &M,
                                            ft::RuntimeFunction FnID);
  void initialize(StringRef HostFilePath = {});
  Module &M;
  IRBuilder<> Builder;
  const std::string ftOffloadInfoName = "ft_offload.info";
  void loadOffloadInfoMetadata(Module &M);

#define FT_TYPE(VarName, InitValue) Type *VarName = nullptr;
#define FT_ARRAY_TYPE(VarName, ElemTy, ArraySize)                             \
  ArrayType *VarName##Ty = nullptr;                                            \
  PointerType *VarName##PtrTy = nullptr;
#define FT_FUNCTION_TYPE(VarName, IsVarArg, ReturnType, ...)                  \
  FunctionType *VarName = nullptr;                                             \
  PointerType *VarName##Ptr = nullptr;
#define FT_STRUCT_TYPE(VarName, StrName, ...)                                 \
  StructType *VarName = nullptr;                                               \
  PointerType *VarName##Ptr = nullptr;
#include "llvm/Frontend/FT/FTKinds.def"
private:
  void initializeTypes(Module &M);
}; // end class FTIRBuilder
} // end namespace llvm
#endif // LLVM_FRONTEND_FT_FTIRBUILDER_H
