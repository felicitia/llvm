cmake_minimum_required(VERSION 3.20.0)
project(DetectTestCompiler C CXX)

include(CheckCCompilerFlag)
include(CheckCXXCompilerFlag)
include(CheckIncludeFile)
include(CheckIncludeFileCXX)

function(write_compiler_information lang)
  set(information "${CMAKE_${lang}_COMPILER}")
  set(information "${information}\\;${CMAKE_${lang}_COMPILER_ID}")
  set(information "${information}\\;${CMAKE_${lang}_COMPILER_VERSION}")
  set(information "${information}\\;${${lang}_FLAGS}")
  set(information "${information}\\;${${lang}_HAS_TSAN_FLAG}")
  set(information "${information}\\;${${lang}_HAS_OMIT_FRAME_POINTER}")
  set(information "${information}\\;${${lang}_HAS_FT_H}")
  file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${lang}CompilerInformation.txt ${information})
endfunction(write_compiler_information)

find_package(FT)
if (NOT FT_Found)
  set(FT_C_FLAGS "-fft")
  set(FT_CXX_FLAGS "-fft")
endif()

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)
find_package(Threads REQUIRED)

set(C_FLAGS "${FT_C_FLAGS} ${CMAKE_THREAD_LIBS_INIT}")
set(CXX_FLAGS "${FT_CXX_FLAGS} ${CMAKE_THREAD_LIBS_INIT}")

check_c_compiler_flag("-fno-omit-frame-pointer" C_HAS_OMIT_FRAME_POINTER)
check_cxx_compiler_flag("-fno-omit-frame-pointer" CXX_HAS_OMIT_FRAME_POINTER)

set(OLD_CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS})
set(CMAKE_REQUIRED_FLAGS "-fsanitize=thread")
check_c_compiler_flag("" C_HAS_TSAN_FLAG)
check_cxx_compiler_flag("" CXX_HAS_TSAN_FLAG)
set(CMAKE_REQUIRED_FLAGS ${OLD_CMAKE_REQUIRED_FLAGS})

# Check if omp.h header exists for the test compiler
check_include_file_cxx(omp.h CXX_HAS_FT_H)
check_include_file(omp.h C_HAS_FT_H)

write_compiler_information(C)
write_compiler_information(CXX)
