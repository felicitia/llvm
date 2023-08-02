//===----- CGFTRuntime.h - Interface to FT Runtimes -----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This provides an abstract class for FT code generation.  Concrete
// subclasses of this implement code generation for specific FT
// runtime libraries.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_CODEGEN_CGFTRUNTIME_H
#define LLVM_CLANG_LIB_CODEGEN_CGFTRUNTIME_H

#include "CGValue.h"
//#include "clang/AST/DeclOpenMP.h"
#include "clang/AST/GlobalDecl.h"
#include "clang/AST/Type.h"
#include "clang/Basic/FTKinds.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Frontend/FT/FTConstants.h"
#include "llvm/Frontend/FT/FTIRBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/ValueHandle.h"
#include "llvm/Support/AtomicOrdering.h"

namespace llvm {
class ArrayType;
class Constant;
class FunctionType;
class GlobalVariable;
class Type;
class Value;
class FTIRBuilder;
} // namespace llvm

namespace clang {
class Expr;
class VarDecl;

namespace CodeGen {
class Address;
class CodeGenFunction;
class CodeGenModule;




class CGFTRuntime {
public:
  llvm::FTIRBuilder &getFTBuilder() { return FTBuilder; }
protected:
  CodeGenModule &CGM;
  llvm::FTIRBuilder FTBuilder;

public:
  explicit CGFTRuntime(CodeGenModule &CGM);
  virtual ~CGFTRuntime() {};

  /// Emit the IR required for a work-group-local variable declaration, and add
  /// an entry to CGF's LocalDeclMap for D.  The base class does this using
  /// CodeGenFunction::EmitStaticVarDecl to emit an internal global for D.
  virtual void emitVoteCall(CodeGenFunction &CGF, llvm::Value * AddrPtr, uint64_t sizeOfType, int whichSide);
  virtual void emitVoteCallDebug(CodeGenFunction &CGF, llvm::Value * AddrPtr, uint64_t sizeOfType, llvm::Value * StrPtr, int lineNo, int whichSide);

};
}
}

#endif
