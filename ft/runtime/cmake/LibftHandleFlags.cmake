#
#//===----------------------------------------------------------------------===//
#//
#// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
#// See https://llvm.org/LICENSE.txt for license information.
#// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#//
#//===----------------------------------------------------------------------===//
#

# Setup the flags correctly for cmake (covert to string)
# Pretty them up (STRIP any beginning and trailing whitespace,
# remove duplicates, remove empty entries)
macro(libft_setup_flags flags)
  if(NOT "${${flags}}" STREQUAL "") # if flags are empty, don't do anything
    set(flags_local)
    list(REMOVE_DUPLICATES ${flags}) # remove duplicates
    list(REMOVE_ITEM ${flags} "") # remove empty items
    libft_list_to_string("${${flags}}" flags_local)
    string(STRIP "${flags_local}" flags_local)
    set(${flags} "${flags_local}")
  endif()
endmacro()

# C++ compiler flags
function(libft_get_cxxflags cxxflags)
  set(flags_local)

  # GCC silently accepts any -Wno-<foo> option, but warns about those options
  # being unrecognized only if the compilation triggers other warnings to be
  # printed. Therefore, check for whether the compiler supports options in the
  # form -W<foo>, and if supported, add the corresponding -Wno-<foo> option.

#  libft_append(flags_local -fno-exceptions LIBFT_HAVE_FNO_EXCEPTIONS_FLAG)
#  libft_append(flags_local -fno-rtti LIBFT_HAVE_FNO_RTTI_FLAG)
#  libft_append(flags_local -Wno-class-memaccess LIBFT_HAVE_WCLASS_MEMACCESS_FLAG)
#  libft_append(flags_local -Wno-covered-switch-default LIBFT_HAVE_WCOVERED_SWITCH_DEFAULT_FLAG)
#  libft_append(flags_local -Wno-frame-address LIBFT_HAVE_WFRAME_ADDRESS_FLAG)
#  libft_append(flags_local -Wno-strict-aliasing LIBFT_HAVE_WSTRICT_ALIASING_FLAG)
#  libft_append(flags_local -Wstringop-overflow=0 LIBFT_HAVE_WSTRINGOP_OVERFLOW_FLAG)
#  libft_append(flags_local -Wno-stringop-truncation LIBFT_HAVE_WSTRINGOP_TRUNCATION_FLAG)
#  libft_append(flags_local -Wno-switch LIBFT_HAVE_WSWITCH_FLAG)
#  libft_append(flags_local -Wno-uninitialized LIBFT_HAVE_WUNINITIALIZED_FLAG)
#  libft_append(flags_local -Wno-return-type-c-linkage LIBFT_HAVE_WRETURN_TYPE_C_LINKAGE_FLAG)
#  libft_append(flags_local -Wno-cast-qual LIBFT_HAVE_WCAST_QUAL_FLAG)
#  libft_append(flags_local -Wno-int-to-void-pointer-cast LIBFT_HAVE_WINT_TO_VOID_POINTER_CAST_FLAG)
#  # libft_append(flags_local -Wconversion LIBFT_HAVE_WCONVERSION_FLAG)
##  libft_append(flags_local /GS LIBFT_HAVE_GS_FLAG)
#  libft_append(flags_local /EHsc LIBFT_HAVE_EHSC_FLAG)
#  libft_append(flags_local /Oy- LIBFT_HAVE_OY__FLAG)
#  if(${IA32} OR ${INTEL64})
#    libft_append(flags_local -mrtm LIBFT_HAVE_MRTM_FLAG)
#  endif()
  # Intel(R) C Compiler flags
#  libft_append(flags_local /Qsafeseh LIBFT_HAVE_QSAFESEH_FLAG)
#  libft_append(flags_local -Qoption,cpp,--extended_float_types LIBFT_HAVE_EXTENDED_FLOAT_TYPES_FLAG)
#  libft_append(flags_local -Qlong_double LIBFT_HAVE_LONG_DOUBLE_FLAG)
#  libft_append(flags_local -Qdiag-disable:177 LIBFT_HAVE_DIAG_DISABLE_177_FLAG)
#  if(${RELEASE_BUILD} OR ${RELWITHDEBINFO_BUILD})
#    libft_append(flags_local -Qinline-min-size=1 LIBFT_HAVE_INLINE_MIN_SIZE_FLAG)
#  endif()
  # Architectural C and C++ flags
#  if(${IA32})
#    if(CMAKE_SIZEOF_VOID_P EQUAL 8)
#      libft_append(flags_local -m32 LIBFT_HAVE_M32_FLAG)
#    endif()
#    libft_append(flags_local /arch:SSE2 LIBFT_HAVE_ARCH_SSE2_FLAG)
#    libft_append(flags_local -msse2 LIBFT_HAVE_MSSE2_FLAG)
#    libft_append(flags_local -falign-stack=maintain-16-byte LIBFT_HAVE_FALIGN_STACK_FLAG)
#  elseif(${MIC})
#    libft_append(flags_local -mmic LIBFT_HAVE_MMIC_FLAG)
#    libft_append(flags_local -ftls-model=initial-exec LIBFT_HAVE_FTLS_MODEL_FLAG)
#    libft_append(flags_local "-opt-streaming-stores never" LIBFT_HAVE_OPT_STREAMING_STORES_FLAG)
#  endif()
#  set(cxxflags_local ${flags_local} ${LIBFT_CXXFLAGS})
#  libft_setup_flags(cxxflags_local)
#  set(${cxxflags} ${cxxflags_local} PARENT_SCOPE)
endfunction()

# Assembler flags
function(libft_get_asmflags asmflags)
  set(asmflags_local)
  # Architectural assembler flags
#  if(${IA32})
#    if(CMAKE_SIZEOF_VOID_P EQUAL 8)
#      libft_append(asmflags_local -m32 LIBFT_HAVE_M32_FLAG)
#    endif()
#    libft_append(asmflags_local /safeseh LIBFT_HAVE_SAFESEH_MASM_FLAG)
#    libft_append(asmflags_local /coff LIBFT_HAVE_COFF_MASM_FLAG)
#  elseif(${MIC})
#    libft_append(asmflags_local -mmic LIBFT_HAVE_MMIC_FLAG)
#  endif()
#  set(asmflags_local ${asmflags_local} ${LIBFT_ASMFLAGS})
#  libft_setup_flags(asmflags_local)
#  set(${asmflags} ${asmflags_local} PARENT_SCOPE)
endfunction()

# Linker flags
function(libft_get_ldflags ldflags)
  set(ldflags_local)
  libft_append(ldflags_local "${CMAKE_LINK_DEF_FILE_FLAG}${CMAKE_CURRENT_BINARY_DIR}/${LIBFT_LIB_NAME}.def"
    IF_DEFINED CMAKE_LINK_DEF_FILE_FLAG)
  libft_append(ldflags_local "${CMAKE_C_OSX_CURRENT_VERSION_FLAG}${LIBFT_VERSION_MAJOR}.${LIBFT_VERSION_MINOR}"
    IF_DEFINED CMAKE_C_OSX_CURRENT_VERSION_FLAG)
  libft_append(ldflags_local "${CMAKE_C_OSX_COMPATIBILITY_VERSION_FLAG}${LIBFT_VERSION_MAJOR}.${LIBFT_VERSION_MINOR}"
    IF_DEFINED CMAKE_C_OSX_COMPATIBILITY_VERSION_FLAG)
  libft_append(ldflags_local -Wl,--as-needed LIBFT_HAVE_AS_NEEDED_FLAG)
  libft_append(ldflags_local "-Wl,--version-script=${LIBFT_SRC_DIR}/exports_so.txt" LIBFT_HAVE_VERSION_SCRIPT_FLAG)
  libft_append(ldflags_local -static-libgcc LIBFT_HAVE_STATIC_LIBGCC_FLAG)
  libft_append(ldflags_local -Wl,-z,noexecstack LIBFT_HAVE_Z_NOEXECSTACK_FLAG)
  libft_append(ldflags_local -no-intel-extensions LIBFT_HAVE_NO_INTEL_EXTENSIONS_FLAG)
  libft_append(ldflags_local -static-intel LIBFT_HAVE_STATIC_INTEL_FLAG)
  libft_append(ldflags_local /SAFESEH LIBFT_HAVE_SAFESEH_FLAG)
  # Architectural linker flags
  if(${IA32})
    if(CMAKE_SIZEOF_VOID_P EQUAL 8)
      libft_append(ldflags_local -m32 LIBFT_HAVE_M32_FLAG)
    endif()
    libft_append(ldflags_local -msse2 LIBFT_HAVE_MSSE2_FLAG)
  elseif(${MIC})
    libft_append(ldflags_local -mmic LIBFT_HAVE_MMIC_FLAG)
    libft_append(ldflags_local -Wl,-x LIBFT_HAVE_X_FLAG)
  endif()
  set(ldflags_local ${ldflags_local} ${LIBFT_LDFLAGS})
  libft_setup_flags(ldflags_local)
  set(${ldflags} ${ldflags_local} PARENT_SCOPE)
endfunction()

# Library flags
function(libft_get_libflags libflags)
  set(libflags_local)
  libft_append(libflags_local "${CMAKE_THREAD_LIBS_INIT}")
  libft_append(libflags_local "${LIBFT_HWLOC_LIBRARY}" LIBFT_USE_HWLOC)
  if(${IA32})
    libft_append(libflags_local -lirc_pic LIBFT_HAVE_IRC_PIC_LIBRARY)
  endif()
  if(MINGW)
    libft_append(libflags_local -lpsapi LIBFT_HAVE_PSAPI)
  endif()
  if(LIBFT_HAVE_SHM_OPEN_WITH_LRT)
    libft_append(libflags_local -lrt)
  endif()
  if(${CMAKE_SYSTEM_NAME} MATCHES "DragonFly|FreeBSD")
    libft_append(libflags_local "-Wl,--no-as-needed" LIBFT_HAVE_AS_NEEDED_FLAG)
    libft_append(libflags_local "-lm")
    libft_append(libflags_local "-Wl,--as-needed" LIBFT_HAVE_AS_NEEDED_FLAG)
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux|NetBSD")
    libft_append(libflags_local -lm)
  endif()
  set(libflags_local ${libflags_local} ${LIBFT_LIBFLAGS})
  libft_setup_flags(libflags_local)
  libft_string_to_list("${libflags_local}" libflags_local_list)
  set(${libflags} ${libflags_local_list} PARENT_SCOPE)
endfunction()

# Fortran flags
function(libft_get_fflags fflags)
  set(fflags_local)
  if(${IA32})
    libft_append(fflags_local -m32 LIBFT_HAVE_M32_FORTRAN_FLAG)
  endif()
  set(fflags_local ${fflags_local} ${LIBFT_FFLAGS})
  libft_setup_flags(fflags_local)
  set(${fflags} ${fflags_local} PARENT_SCOPE)
endfunction()

# Perl generate-defs.pl flags (For Windows only)
function(libft_get_gdflags gdflags)
  set(gdflags_local)
  if(${IA32})
    set(libft_gdflag_arch arch_32)
    libft_append(gdflags_local "-D IS_IA_ARCH")
  elseif(${INTEL64})
    set(libft_gdflag_arch arch_32e)
    libft_append(gdflags_local "-D IS_IA_ARCH")
  else()
    set(libft_gdflag_arch arch_${LIBFT_ARCH})
  endif()
#  libft_append(gdflags_local "-D ${libft_gdflag_arch}")
#  libft_append(gdflags_local "-D msvc_compat")
#  libft_append(gdflags_local "-D norm" NORMAL_LIBRARY)
#  libft_append(gdflags_local "-D prof" PROFILE_LIBRARY)
#  libft_append(gdflags_local "-D stub" STUBS_LIBRARY)
#  libft_append(gdflags_local "-D HAVE_QUAD" LIBFT_USE_QUAD_PRECISION)
  libft_append(gdflags_local "-D USE_DEBUGGER" LIBFT_USE_DEBUGGER)
#  if(${DEBUG_BUILD} OR ${RELWITHDEBINFO_BUILD})
#    libft_append(gdflags_local "-D KMP_DEBUG")
#  endif()
  set(${gdflags} ${gdflags_local} PARENT_SCOPE)
endfunction()
