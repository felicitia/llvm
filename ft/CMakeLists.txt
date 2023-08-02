cmake_minimum_required(VERSION 3.20.0)

set(LLVM_COMMON_CMAKE_UTILS ${CMAKE_CURRENT_SOURCE_DIR}/../cmake)

# Add path for custom modules
list(INSERT CMAKE_MODULE_PATH 0
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
  "${LLVM_COMMON_CMAKE_UTILS}/Modules"
  )

# llvm/runtimes/ will set FT_STANDALONE_BUILD.
#if (FT_STANDALONE_BUILD OR "${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_SOURCE_DIR}")
set(FT_STANDALONE_BUILD TRUE)
project(ft C CXX ASM)

# Must go below project(..)
include(GNUInstallDirs)

#if (FT_STANDALONE_BUILD)
# CMAKE_BUILD_TYPE was not set, default to Release.
if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

# Group common settings.
set(FT_ENABLE_WERROR FALSE CACHE BOOL
  "Enable -Werror flags to turn warnings into errors for supporting compilers.")
set(FT_LIBDIR_SUFFIX "" CACHE STRING
  "Suffix of lib installation directory, e.g. 64 => lib64")
# Do not use FT_LIBDIR_SUFFIX directly, use FT_INSTALL_LIBDIR.
set(FT_INSTALL_LIBDIR "lib${FT_LIBDIR_SUFFIX}")

# Group test settings.
set(FT_TEST_C_COMPILER ${CMAKE_C_COMPILER} CACHE STRING
  "C compiler to use for testing OpenMP runtime libraries.")
set(FT_TEST_CXX_COMPILER ${CMAKE_CXX_COMPILER} CACHE STRING
  "C++ compiler to use for testing OpenMP runtime libraries.")
set(FT_LLVM_TOOLS_DIR "" CACHE PATH "Path to LLVM tools for testing.")

set(CMAKE_CXX_STANDARD 17 CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED NO)
set(CMAKE_CXX_EXTENSIONS NO)

# Check and set up common compiler flags.
include(config-ix)
#include(HandleOpenMPOptions)

# Set up testing infrastructure.
#include(OpenMPTesting)

#set(FT_TEST_FLAGS "" CACHE STRING
#  "Extra compiler flags to send to the test compiler.")
#set(FT_TEST_FT_FLAGS ${FT_TEST_COMPILER_FT_FLAGS} CACHE STRING
#  "OpenMP compiler flag to use for testing OpenMP runtime libraries.")

#if(${FT_STANDALONE_BUILD})
set(LIBFT_HEADERS_INSTALL_PATH "${CMAKE_INSTALL_INCLUDEDIR}")
#else()
#  include(GetClangResourceDir)
#  get_clang_resource_dir(LIBFT_HEADERS_INSTALL_PATH SUBDIR include)
#endif()

# Build host runtime library, after LIBFTTARGET variables are set since they are needed
# to enable time profiling support in the OpenMP runtime.
add_subdirectory(runtime)


# Build libompd.so
#add_subdirectory(libompd)

# Build documentation
#add_subdirectory(docs)

# Now that we have seen all testsuites, create the check-openmp target.
#construct_check_openmp_target()
