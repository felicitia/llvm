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

# Delete artifacts directory if it exists
if [ -d $ARTIFACTS_DIR ]; then
    rm -rf $ARTIFACTS_DIR
fi

# Create artifacts directory
mkdir -p $ARTIFACTS_DIR

# Step 1: Generate intermediate `.ll`
clang $INCLUDE_PATHS -O${OPTIMIZATION_LEVEL:1} -g -emit-llvm -S --target=riscv64 $INPUT_C_FILE -o $INTERMEDIATE_LL_FILE -mabi=lp64d -fno-addrsig -fPIC -DDEBUG

# Step 2: Apply LLVM pass to generate transformed `.ll`
$LLVM_BIN_PATH/opt -S -passes=hpsc-cfa $INTERMEDIATE_LL_FILE -o $TRANSFORMED_LL_FILE

# Step 3: Generate `.s` for original and transformed `.ll`
llc -march=riscv64 -filetype=asm $INTERMEDIATE_LL_FILE -o $ASM_FILE
llc -march=riscv64 -filetype=asm $TRANSFORMED_LL_FILE -o $TRANSFORMED_ASM_FILE

# Step 4: Generate `.o` and executables for original and transformed `.ll`
$RISCV_GCC -c -fPIC $ASM_FILE -o $OBJECT_FILE
$RISCV_GCC -static -o $EXECUTABLE_FILE $OBJECT_FILE

$RISCV_GCC -c -fPIC $TRANSFORMED_ASM_FILE -o $TRANSFORMED_OBJECT_FILE
$RISCV_GCC -static -o $TRANSFORMED_EXECUTABLE_FILE $TRANSFORMED_OBJECT_FILE

# Step 5: Move graph.dot to artifacts directory if it exists
if [ -f graph.dot ]; then
    mv graph.dot $ARTIFACTS_DIR
fi

echo "RISC-V executables generated successfully: $EXECUTABLE_FILE and $TRANSFORMED_EXECUTABLE_FILE"
