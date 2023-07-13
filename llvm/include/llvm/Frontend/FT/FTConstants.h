//===- FTConstants.h - FT related constants and helpers ------ C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
///
/// This file defines constans and helpers used when dealing with FT.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_FRONTEND_FT_FTCONSTANTS_H
#define LLVM_FRONTEND_FT_FTCONSTANTS_H

#include "llvm/ADT/BitmaskEnum.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/Frontend/FT/FT.h.inc"
//#include "llvm/Frontend/FT/FT.inc"

namespace llvm {
namespace ft {
LLVM_ENABLE_BITMASK_ENUMS_IN_NAMESPACE();

#if 0
/// IDs for all Internal Control Variables (ICVs).
enum class InternalControlVar {
#define ICV_DATA_ENV(Enum, ...) Enum,
#include "llvm/Frontend/FT/FTKinds.def"
};

#define ICV_DATA_ENV(Enum, ...)                                                \
  constexpr auto Enum = ft::InternalControlVar::Enum;
#include "llvm/Frontend/FT/FTKinds.def"

enum class ICVInitValue {
#define ICV_INIT_VALUE(Enum, Name) Enum,
#include "llvm/Frontend/FT/FTKinds.def"
};

#define ICV_INIT_VALUE(Enum, Name)                                             \
  constexpr auto Enum = ft::ICVInitValue::Enum;
#include "llvm/Frontend/FT/FTKinds.def"
#endif

/// IDs for all ft runtime library (RTL) functions.
#if 0
enum class RuntimeFunction {
#define FT_RTL(Enum, ...) Enum,
#include "llvm/Frontend/FT/FTKinds.def"
};

#define FT_RTL(Enum, ...) constexpr auto Enum = ft::RuntimeFunction::Enum;
#include "llvm/Frontend/FT/FTKinds.def"

/// IDs for the different default kinds.
enum class DefaultKind {
#define FT_DEFAULT_KIND(Enum, Str) Enum,
#include "llvm/Frontend/FT/FTKinds.def"
};

#define FT_DEFAULT_KIND(Enum, ...)                                            \
  constexpr auto Enum = ft::DefaultKind::Enum;
#include "llvm/Frontend/FT/FTKinds.def"

/// IDs for all ft runtime library ident_t flag encodings (see
/// their defintion in openmp/runtime/src/kmp.h).
enum class FTIdentFlag {
#define FT_IDENT_FLAG(Enum, Str, Value) Enum = Value,
#include "llvm/Frontend/FT/FTKinds.def"
  LLVM_MARK_AS_BITMASK_ENUM(0x7FFFFFFF)
};

#define FT_IDENT_FLAG(Enum, ...) constexpr auto Enum = ft::FTIdentFlag::Enum;
#include "llvm/Frontend/FT/FTKinds.def"
#endif



} // end namespace ft

} // end namespace llvm

#endif // LLVM_FRONTEND_OPENMP_FTCONSTANTS_H
