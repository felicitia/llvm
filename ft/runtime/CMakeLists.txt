#
#//===----------------------------------------------------------------------===//
#//
#// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
#// See https://llvm.org/LICENSE.txt for license information.
#// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#//
#//===----------------------------------------------------------------------===//
#

if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_SOURCE_DIR}")
  message(FATAL_ERROR "Direct configuration not supported, please use parent directory!")
endif()

# Add cmake directory to search for custom cmake functions
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

# These include files are in the cmake/ subdirectory
include(LibftUtils)
include(LibftGetArchitecture)
include(LibftHandleFlags)

# Determine the target architecture
if(${FT_STANDALONE_BUILD})
  # If adding a new architecture, take a look at cmake/LibompGetArchitecture.cmake
  libft_get_architecture(LIBFT_DETECTED_ARCH)
  set(LIBFT_ARCH ${LIBFT_DETECTED_ARCH} CACHE STRING
    "The architecture to build for (x86_64/i386/arm/ppc64/ppc64le/aarch64/mic/mips/mips64/riscv64/loongarch64).")
  # Should assertions be enabled?  They are on by default.
  set(LIBFT_ENABLE_ASSERTIONS TRUE CACHE BOOL
    "enable assertions?")
  string(TOUPPER "${CMAKE_BUILD_TYPE}" uppercase_CMAKE_BUILD_TYPE)
else() # Part of LLVM build
  # Determine the native architecture from LLVM.
  string(TOLOWER "${LLVM_TARGET_ARCH}" LIBFT_NATIVE_ARCH)
  if( LIBFT_NATIVE_ARCH STREQUAL "host" )
    string(REGEX MATCH "^[^-]*" LIBFT_NATIVE_ARCH ${LLVM_HOST_TRIPLE})
  endif ()
  if(LIBFT_NATIVE_ARCH MATCHES "i[2-6]86")
    set(LIBFT_ARCH i386)
  elseif(LIBFT_NATIVE_ARCH STREQUAL "x86")
    set(LIBFT_ARCH i386)
  elseif(LIBFT_NATIVE_ARCH STREQUAL "amd64")
    set(LIBFT_ARCH x86_64)
  elseif(LIBFT_NATIVE_ARCH STREQUAL "x86_64")
    set(LIBFT_ARCH x86_64)
  elseif(LIBFT_NATIVE_ARCH MATCHES "powerpc64le")
    set(LIBFT_ARCH ppc64le)
  elseif(LIBFT_NATIVE_ARCH MATCHES "powerpc")
    set(LIBFT_ARCH ppc64)
  elseif(LIBFT_NATIVE_ARCH MATCHES "aarch64")
    set(LIBFT_ARCH aarch64)
  elseif(LIBFT_NATIVE_ARCH MATCHES "arm64")
    set(LIBFT_ARCH aarch64)
  elseif(LIBFT_NATIVE_ARCH MATCHES "arm")
    set(LIBFT_ARCH arm)
  elseif(LIBFT_NATIVE_ARCH MATCHES "riscv64")
    set(LIBFT_ARCH riscv64)
  elseif(LIBFT_NATIVE_ARCH MATCHES "loongarch64")
    set(LIBFT_ARCH loongarch64)
#  else()
#    # last ditch effort
#    libft_get_architecture(LIBFT_ARCH)
  endif ()
  set(LIBFT_ENABLE_ASSERTIONS ${LLVM_ENABLE_ASSERTIONS})
endif()

# Time profiling support
set(LIBFT_PROFILING_SUPPORT ${FT_ENABLE_LIBFT_PROFILING})

# FUJITSU A64FX is a special processor because its cache line size is 256.
# We need to pass this information into kmp_config.h.
if(LIBFT_ARCH STREQUAL "aarch64")
  libft_is_aarch64_a64fx(LIBFT_DETECT_AARCH64_A64FX)
  if (LIBFT_DETECT_AARCH64_A64FX)
    set(LIBFT_ARCH "aarch64_a64fx")
    set(LIBFT_ARCH_AARCH64_A64FX TRUE)
  endif()
endif()

libft_check_variable(LIBFT_ARCH 32e x86_64 32 i386 arm ppc64 ppc64le aarch64 aarch64_a64fx mic mips mips64 riscv64 loongarch64)

set(LIBFT_LIB_TYPE normal CACHE STRING
  "Performance,Profiling,Stubs library (normal/profile/stubs)")
libft_check_variable(LIBFT_LIB_TYPE normal profile stubs)
# Set the OpenMP Year and Month associated with version
set(LIBFT_OMP_YEAR_MONTH 201611)
set(LIBFT_MIC_ARCH knc CACHE STRING
  "Intel(R) Many Integrated Core Architecture (Intel(R) MIC Architecture) (knf/knc).  Ignored if not Intel(R) MIC Architecture build.")
if("${LIBFT_ARCH}" STREQUAL "mic")
  libft_check_variable(LIBFT_MIC_ARCH knf knc)
endif()

# - Support for universal fat binary builds on Mac
# - Having this extra variable allows people to build this library as a universal library
#   without forcing a universal build of the llvm/clang compiler.
set(LIBFT_OSX_ARCHITECTURES "${CMAKE_OSX_ARCHITECTURES}" CACHE STRING
  "For Mac builds, semicolon separated list of architectures to build for universal fat binary.")
set(CMAKE_OSX_ARCHITECTURES ${LIBFT_OSX_ARCHITECTURES})

# Should @rpath be used for dynamic libraries on Mac?
# The if(NOT DEFINED) is there to guard a cached value of the variable if one
# exists so there is no interference with what the user wants.  Also, no cache entry
# is created so there are no inadvertant effects on other parts of LLVM.
if(NOT DEFINED CMAKE_MACOSX_RPATH)
  set(CMAKE_MACOSX_RPATH TRUE)
endif()

# User specified flags.  These are appended to the configured flags.
set(LIBFT_CXXFLAGS "" CACHE STRING
  "Appended user specified C++ compiler flags.")
set(LIBFT_CPPFLAGS "" CACHE STRING
  "Appended user specified C preprocessor flags.")
set(LIBFT_ASMFLAGS "" CACHE STRING
  "Appended user specified assembler flags.")
set(LIBFT_LDFLAGS "" CACHE STRING
  "Appended user specified linker flags.")
set(LIBFT_LIBFLAGS "" CACHE STRING
  "Appended user specified linked libs flags. (e.g., -lm)")

# Should the libft library and generated headers be copied into the original source exports/ directory
# Turning this to FALSE aids parallel builds to not interfere with each other.
# Currently, the testsuite module expects the just built OpenMP library to be located inside the exports/
# directory.  TODO: have testsuite run under llvm-lit directly.  We can then get rid of copying to exports/
set(LIBFT_COPY_EXPORTS FALSE CACHE STRING
  "Should exports be copied into source exports/ directory?")

# Get the build number from kmp_version.cpp
#libft_get_build_number("${CMAKE_CURRENT_SOURCE_DIR}" LIBFT_VERSION_BUILD)
#math(EXPR LIBFT_VERSION_BUILD_YEAR "${LIBFT_VERSION_BUILD}/10000")
#math(EXPR LIBFT_VERSION_BUILD_MONTH_DAY "${LIBFT_VERSION_BUILD}%10000")

# Currently don't record any timestamps
set(LIBFT_BUILD_DATE "No_Timestamp")

# Architecture
set(IA32 FALSE)
set(INTEL64 FALSE)
set(ARM FALSE)
set(AARCH64 FALSE)
set(AARCH64_A64FX FALSE)
set(PPC64BE FALSE)
set(PPC64LE FALSE)
set(PPC64 FALSE)
set(MIC FALSE)
set(MIPS64 FALSE)
set(MIPS FALSE)
set(RISCV64 FALSE)
set(LOONGARCH64 FALSE)
if("${LIBFT_ARCH}" STREQUAL "i386" OR "${LIBFT_ARCH}" STREQUAL "32")    # IA-32 architecture
  set(IA32 TRUE)
elseif("${LIBFT_ARCH}" STREQUAL "x86_64" OR "${LIBFT_ARCH}" STREQUAL "32e") # Intel(R) 64 architecture
  set(INTEL64 TRUE)
elseif("${LIBFT_ARCH}" STREQUAL "arm") # ARM architecture
  set(ARM TRUE)
elseif("${LIBFT_ARCH}" STREQUAL "ppc64") # PPC64BE architecture
  set(PPC64BE TRUE)
  set(PPC64 TRUE)
elseif("${LIBFT_ARCH}" STREQUAL "ppc64le") # PPC64LE architecture
  set(PPC64LE TRUE)
  set(PPC64 TRUE)
elseif("${LIBFT_ARCH}" STREQUAL "aarch64") # AARCH64 architecture
  set(AARCH64 TRUE)
elseif("${LIBFT_ARCH}" STREQUAL "aarch64_a64fx") # AARCH64_A64FX architecture
  set(AARCH64_A64FX TRUE)
elseif("${LIBFT_ARCH}" STREQUAL "mic") # Intel(R) Many Integrated Core Architecture
  set(MIC TRUE)
elseif("${LIBFT_ARCH}" STREQUAL "mips") # MIPS architecture
    set(MIPS TRUE)
elseif("${LIBFT_ARCH}" STREQUAL "mips64") # MIPS64 architecture
    set(MIPS64 TRUE)
elseif("${LIBFT_ARCH}" STREQUAL "riscv64") # RISCV64 architecture
    set(RISCV64 TRUE)
elseif("${LIBFT_ARCH}" STREQUAL "loongarch64") # LoongArch64 architecture
    set(LOONGARCH64 TRUE)
endif()

# Set some flags based on build_type
set(RELEASE_BUILD FALSE)
set(DEBUG_BUILD FALSE)
set(RELWITHDEBINFO_BUILD FALSE)
set(MINSIZEREL_BUILD FALSE)
if("${uppercase_CMAKE_BUILD_TYPE}" STREQUAL "RELEASE")
  set(RELEASE_BUILD TRUE)
elseif("${uppercase_CMAKE_BUILD_TYPE}" STREQUAL "DEBUG")
  set(DEBUG_BUILD TRUE)
elseif("${uppercase_CMAKE_BUILD_TYPE}" STREQUAL "RELWITHDEBINFO")
  set(RELWITHDEBINFO_BUILD TRUE)
elseif("${uppercase_CMAKE_BUILD_TYPE}" STREQUAL "MINSIZEREL")
  set(MINSIZEREL_BUILD TRUE)
endif()

# Include itt notify interface?
set(LIBFT_USE_ITT_NOTIFY TRUE CACHE BOOL
  "Enable ITT notify?")

# normal, profile, stubs library.
set(NORMAL_LIBRARY FALSE)
set(STUBS_LIBRARY FALSE)
set(PROFILE_LIBRARY FALSE)
if("${LIBFT_LIB_TYPE}" STREQUAL "normal")
  set(NORMAL_LIBRARY TRUE)
elseif("${LIBFT_LIB_TYPE}" STREQUAL "profile")
  set(PROFILE_LIBRARY TRUE)
elseif("${LIBFT_LIB_TYPE}" STREQUAL "stubs")
  set(STUBS_LIBRARY TRUE)
endif()

# Setting directory names
set(LIBFT_BASE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(LIBFT_SRC_DIR ${LIBFT_BASE_DIR}/src)
set(LIBFT_TOOLS_DIR ${LIBFT_BASE_DIR}/tools)
set(LIBFT_INC_DIR ${LIBFT_SRC_DIR}/include)
set(LIBFT_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

# Enabling Fortran if it is needed
# Enable MASM Compiler if it is needed (Windows only)
if(WIN32)
  enable_language(ASM_MASM)
endif()

# Getting legal type/arch
libft_get_legal_type(LIBFT_LEGAL_TYPE)
libft_get_legal_arch(LIBFT_LEGAL_ARCH)

# Compiler flag checks, library checks, threading check, etc.
include(config-ix)

# Build code that allows the OpenMP library to conveniently interface with debuggers
set(LIBFT_USE_DEBUGGER FALSE CACHE BOOL
  "Enable debugger interface code?")


# - stats-gathering enables OpenMP stats where things like the number of
# parallel regions, clock ticks spent in particular openmp regions are recorded.
set(LIBFT_STATS FALSE CACHE BOOL
  "Stats-Gathering functionality?")
if(LIBFT_STATS AND (NOT LIBFT_HAVE_STATS))
  libft_error_say("Stats-gathering functionality requested but not available")
endif()
# The stats functionality requires the std c++ library
if(LIBFT_STATS)
  set(LIBFT_USE_STDCPPLIB TRUE)
endif()

# Shared library can be switched to a static library
set(LIBFT_ENABLE_SHARED TRUE CACHE BOOL
  "Shared library instead of static library?")

# Setting final library name
set(LIBFT_DEFAULT_LIB_NAME libft)
if(${PROFILE_LIBRARY})
  set(LIBFT_DEFAULT_LIB_NAME ${LIBFT_DEFAULT_LIB_NAME}prof)
endif()
if(${STUBS_LIBRARY})
  set(LIBFT_DEFAULT_LIB_NAME ${LIBFT_DEFAULT_LIB_NAME}stubs)
endif()
set(LIBFT_LIB_NAME ${LIBFT_DEFAULT_LIB_NAME} CACHE STRING "Base FT library name")
if (FT_MSVC_NAME_SCHEME)
  # MSVC_TOOLS_VERSION corresponds to the version of the VC++ toolset.
  set(MSVC_TOOLS_VERSION 140)
  set(LIBFT_LIB_NAME ${LIBFT_LIB_NAME}${MSVC_TOOLS_VERSION}.${LIBFT_ARCH})
endif()

if(${LIBFT_ENABLE_SHARED})
  set(LIBFT_LIBRARY_SUFFIX ${CMAKE_SHARED_LIBRARY_SUFFIX})
  set(LIBFT_LIBRARY_KIND SHARED)
  set(LIBFT_INSTALL_KIND LIBRARY)
else()
  set(LIBFT_LIBRARY_SUFFIX ${CMAKE_STATIC_LIBRARY_SUFFIX})
  set(LIBFT_LIBRARY_KIND STATIC)
  set(LIBFT_INSTALL_KIND ARCHIVE)
endif()

set(LIBFT_LIB_FILE ${LIBFT_LIB_NAME}${LIBFT_LIBRARY_SUFFIX})

# Optional backwards compatibility aliases.
set(LIBFT_INSTALL_ALIASES TRUE CACHE BOOL
  "Install libgft and libift5 library aliases for backwards compatibility")

# Print configuration after all variables are set.
if(${FT_STANDALONE_BUILD})
  libft_say("Operating System     -- ${CMAKE_SYSTEM_NAME}")
  libft_say("Target Architecture  -- ${LIBFT_ARCH}")
  if(${MIC})
    libft_say("Intel(R) MIC Architecture    -- ${LIBFT_MIC_ARCH}")
  endif()
  libft_say("Build Type           -- ${CMAKE_BUILD_TYPE}")
  libft_say("Library Kind         -- ${LIBFT_LIBRARY_KIND}")
  libft_say("Library Type         -- ${LIBFT_LIB_TYPE}")
  # will say development if all zeros
#  if(${LIBFT_VERSION_BUILD} STREQUAL 00000000)
#    set(LIBFT_BUILD Development)
#  else()
#    set(LIBFT_BUILD ${LIBFT_VERSION_BUILD})
#  endif()
  set(${LIBFT_BUILD} "1.0")
  libft_say("Build                -- ${LIBFT_BUILD}")
  libft_say("Use Stats-gathering  -- ${LIBFT_STATS}")
  libft_say("Use Debugger-support -- ${LIBFT_USE_DEBUGGER}")
#  libft_say("Use ITT notify       -- ${LIBFT_USE_ITT_NOTIFY}")
#  libft_say("Use OMPT-support     -- ${LIBFT_OMPT_SUPPORT}")
#  if(${LIBFT_OMPT_SUPPORT})
#    libft_say("Use OMPT-optional  -- ${LIBFT_OMPT_OPTIONAL}")
#  endif()
#  libft_say("Use OMPD-support     -- ${LIBFT_OMPD_SUPPORT}")
#  libft_say("Use Adaptive locks   -- ${LIBFT_USE_ADAPTIVE_LOCKS}")
#  libft_say("Use quad precision   -- ${LIBFT_USE_QUAD_PRECISION}")
#  libft_say("Use Hwloc library    -- ${LIBFT_USE_HWLOC}")
#  libft_say("Use OMPX-taskgraph   -- ${LIBFT_OMPX_TASKGRAPH}")
endif()

add_subdirectory(src)
#add_subdirectory(test)

# make these variables available for tools:
set(LIBFT_LIBRARY_DIR ${LIBFT_LIBRARY_DIR} PARENT_SCOPE)
set(LIBFT_INCLUDE_DIR ${LIBFT_INCLUDE_DIR} PARENT_SCOPE)
set(LIBFT_OMP_TOOLS_INCLUDE_DIR ${LIBFT_OMP_TOOLS_INCLUDE_DIR} PARENT_SCOPE)
# make these variables available for tools/libftd:
set(LIBFT_SRC_DIR ${LIBFT_SRC_DIR} PARENT_SCOPE)
set(LIBFT_OMPD_SUPPORT ${LIBFT_OMPD_SUPPORT} PARENT_SCOPE)
