#
#//===----------------------------------------------------------------------===//
#//
#// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
#// See https://llvm.org/LICENSE.txt for license information.
#// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#//
#//===----------------------------------------------------------------------===//
#

include(ExtendPath)

# Configure ft.h
configure_file(${LIBFT_INC_DIR}/ft.h.var ft.h @ONLY)

# Set the -D definitions for all sources
# UNICODE and _UNICODE are set in LLVM's CMake system.  They affect the
# ittnotify code and should only be set when compiling ittnotify_static.cpp
# on Windows (done below).
# TODO: Fix the UNICODE usage in ittnotify code for Windows.
remove_definitions(-DUNICODE -D_UNICODE)
#libft_get_definitions_flags(LIBFT_CONFIGURED_DEFINITIONS_FLAGS)
#add_definitions(${LIBFT_CONFIGURED_DEFINITIONS_FLAGS})

# Set the -I includes for all sources
include_directories(
  ${CMAKE_CURRENT_BINARY_DIR}
  ${LIBFT_SRC_DIR}
  ${LIBFT_INC_DIR}
)

# Building with time profiling support requires LLVM directory includes.
if(LIBFT_PROFILING_SUPPORT)
  include_directories(
    ${LLVM_MAIN_INCLUDE_DIR}
    ${LLVM_INCLUDE_DIR}
  )
endif()

# Getting correct source files to build library
set(LIBFT_CXXFILES)
set(LIBFT_ASMFILES)
set(LIBFT_GNUASMFILES)
  # Get C++ files
set(LIBFT_CXXFILES
    kft_lib.cpp
    ft_lib.cpp   
)

set(LIBFT_SOURCE_FILES ${LIBFT_CXXFILES} ${LIBFT_ASMFILES} ${LIBFT_FTASMFILES})

# Get compiler and assembler flags
set(LIBFT_CONFIGURED_CXXFLAGS)
#libft_get_cxxflags(LIBFT_CONFIGURED_CXXFLAGS)
set(LIBFT_CONFIGURED_ASMFLAGS)
#libft_get_asmflags(LIBFT_CONFIGURED_ASMFLAGS)
# Set the compiler flags for each type of source
set_source_files_properties(${LIBFT_CXXFILES} PROPERTIES COMPILE_FLAGS "${LIBFT_CONFIGURED_CXXFLAGS}")
set_source_files_properties(${LIBFT_ASMFILES} ${LIBFT_GNUASMFILES} PROPERTIES COMPILE_FLAGS "${LIBFT_CONFIGURED_ASMFLAGS}")

# We neither need (nor want) the standard C++ library dependency even though we compile c++ files.
if(NOT ${LIBFT_USE_STDCPPLIB})
  set(LIBFT_LINKER_LANGUAGE C)
  set(CMAKE_CXX_IMPLICIT_LINK_LIBRARIES)
else()
  set(LIBFT_LINKER_LANGUAGE CXX)
endif()

if(UNIX)
  set(LIBFT_DL_LIBS ${CMAKE_DL_LIBS})
endif()

# Add the FT library
libft_get_ldflags(LIBFT_CONFIGURED_LDFLAGS)

libft_get_libflags(LIBFT_CONFIGURED_LIBFLAGS)

# Build libft library. Add LLVMSupport dependency if building in-tree with libfttarget profiling enabled.
if(FT_STANDALONE_BUILD)
  add_library(ft ${LIBFT_LIBRARY_KIND} ${LIBFT_SOURCE_FILES})
  # Linking command will include libraries in LIBFT_CONFIGURED_LIBFLAGS
  target_link_libraries(ft ${LIBFT_CONFIGURED_LIBFLAGS} ${LIBFT_DL_LIBS})
else()
  add_llvm_library(ft ${LIBFT_LIBRARY_KIND} ${LIBFT_SOURCE_FILES} PARTIAL_SOURCES_INTENDED
    LINK_LIBS ${LIBFT_CONFIGURED_LIBFLAGS} ${LIBFT_DL_LIBS}
    LINK_COMPONENTS Support
    )
  # libft must be a C++ library such that it can link libLLVMSupport
  set(LIBFT_LINKER_LANGUAGE CXX)
endif()

set_target_properties(ft PROPERTIES
  PREFIX "" SUFFIX "" OUTPUT_NAME "${LIBFT_LIB_FILE}"
  LINK_FLAGS "${LIBFT_CONFIGURED_LDFLAGS}"
  LINKER_LANGUAGE ${LIBFT_LINKER_LANGUAGE}
)

# Get the library's location within the build tree for the unit tester
get_target_property(LIBFT_LIBRARY_DIR ft LIBRARY_OUTPUT_DIRECTORY)
if(NOT LIBFT_LIBRARY_DIR)
  set(LIBFT_LIBRARY_DIR ${CMAKE_CURRENT_BINARY_DIR})
  set(LIBFT_LIBRARY_DIR ${CMAKE_CURRENT_BINARY_DIR} PARENT_SCOPE)
else()
  set(LIBFT_LIBRARY_DIR ${LIBFT_LIBRARY_DIR} PARENT_SCOPE)
endif()
set(LIBFT_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR} PARENT_SCOPE)

# Add symbolic links to libft
if(NOT WIN32)
  add_custom_command(TARGET ft POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E create_symlink ${LIBFT_LIB_FILE}
      libgft${LIBFT_LIBRARY_SUFFIX}
    COMMAND ${CMAKE_COMMAND} -E create_symlink ${LIBFT_LIB_FILE}
      libift5${LIBFT_LIBRARY_SUFFIX}
    WORKING_DIRECTORY ${LIBFT_LIBRARY_DIR}
  )
endif()
set(LIBFT_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR} PARENT_SCOPE)

# Move files to exports/ directory if requested
if(${LIBFT_COPY_EXPORTS})
  include(LibftExports)
endif()

# `omp` needs to be exported if in-tree build.
set(export_to_llvmexports)
if (NOT FT_STANDALONE_BUILD)
  get_target_export_arg(ft LLVM export_to_llvmexports)
  set_property(GLOBAL APPEND PROPERTY LLVM_EXPORTS ft)
endif()

# Install rules
# We want to install libft in ${DESTDIR}/${CMAKE_INSTALL_FULL_LIBDIR}
# We want to install headers in ${DESTDIR}/${CMAKE_INSTALL_FULL_INCLUDEDIR}

install(TARGETS ft ${export_to_llvmexports} ${LIBFT_INSTALL_KIND} DESTINATION "${FT_INSTALL_LIBDIR}")

if(${LIBFT_INSTALL_ALIASES})
  # Create aliases (symlinks) of the library for backwards compatibility
  set(LIBFT_ALIASES "libgft;libift5")
  foreach(alias IN LISTS LIBFT_ALIASES)
    extend_path(outdir "${CMAKE_INSTALL_PREFIX}" "${FT_INSTALL_LIBDIR}")
    install(CODE "execute_process(COMMAND \"\${CMAKE_COMMAND}\" -E create_symlink \"${LIBFT_LIB_FILE}\"
      \"${alias}${LIBFT_LIBRARY_SUFFIX}\" WORKING_DIRECTORY
      \"\$ENV{DESTDIR}${outdir}\")")
  endforeach()
endif()
install(
  FILES
  ${CMAKE_CURRENT_BINARY_DIR}/ft.h
  DESTINATION ${LIBFT_HEADERS_INSTALL_PATH}
)
