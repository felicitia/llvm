#!/bin/bash

# Variables
INPUT_C_FILE="/home/yixue/isi-llvm/llvm/hpsc_test_apps/cfa_loop_demo.c"
OPTIMIZATION_LEVEL="O1"
BASE_NAME=$(basename $INPUT_C_FILE .c)
ARTIFACTS_DIR=$(dirname $INPUT_C_FILE)/${BASE_NAME}_riscv64

INTERMEDIATE_LL_FILE="$ARTIFACTS_DIR/${BASE_NAME}_${OPTIMIZATION_LEVEL}_riscv.ll"
TRANSFORMED_LL_FILE="$ARTIFACTS_DIR/${BASE_NAME}_transformed_riscv64.ll"

ASM_FILE="$ARTIFACTS_DIR/${BASE_NAME}_riscv.s"
TRANSFORMED_ASM_FILE="$ARTIFACTS_DIR/${BASE_NAME}_riscv_transformed.s"

OBJECT_FILE="$ARTIFACTS_DIR/${BASE_NAME}_riscv.o"
TRANSFORMED_OBJECT_FILE="$ARTIFACTS_DIR/${BASE_NAME}_riscv_transformed.o"

EXECUTABLE_FILE="$ARTIFACTS_DIR/${BASE_NAME}_riscv"
TRANSFORMED_EXECUTABLE_FILE="$ARTIFACTS_DIR/${BASE_NAME}_riscv_transformed"

# Paths
CLANG_PATH="/opt/riscv64"
LLVM_BIN_PATH="/home/yixue/isi-llvm/build/bin"
RISCV_GCC="riscv64-unknown-linux-gnu-gcc"

# Include paths
INCLUDE_PATHS="-I$CLANG_PATH/include -I$CLANG_PATH/riscv64-unknown-linux-gnu/include -I$CLANG_PATH/sysroot/usr/include"