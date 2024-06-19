#!/bin/bash

# Variables
INPUT_C_FILE="/home/yixue/isi-llvm/llvm/hpsc_test_apps/cfa_loop_demo.c"
OPTIMIZATION_LEVEL="O1"
BASE_NAME=$(basename $INPUT_C_FILE .c)
ARTIFACTS_DIR=$(dirname $INPUT_C_FILE)/${BASE_NAME}_x86
INTERMEDIATE_LL_FILE="$ARTIFACTS_DIR/${BASE_NAME}_${OPTIMIZATION_LEVEL}_x86.ll"
TRANSFORMED_LL_FILE="$ARTIFACTS_DIR/${BASE_NAME}_transformed_x86.ll"
EXECUTABLE_FILE="$ARTIFACTS_DIR/${BASE_NAME}_x86"
TRANSFORMED_EXECUTABLE_FILE="$ARTIFACTS_DIR/${BASE_NAME}_x86_transformed"

# Paths
LLVM_BIN_PATH="/home/yixue/isi-llvm/build/bin"

# Display mode instructions and prompt user for input
echo "Specify the operational mode of the pass:"
echo "1 - Debug mode with signature checks"
echo "2 - Debug mode without signature checks to generate graph.dot"
echo "3 - Production mode with signature checks"
read -p "Enter mode (1, 2, or 3): " MODE

# Check if mode is provided and is one of the expected values, otherwise default to mode 2
if [ -z "$MODE" ] || [[ ! "$MODE" =~ ^[123]$ ]]; then
  echo "Invalid mode selected. Defaulting to mode 1."
  MODE=1
fi

# Create artifacts directory
mkdir -p $ARTIFACTS_DIR

# Step 1: Generate `.ll`
clang -O${OPTIMIZATION_LEVEL:1} -g -emit-llvm -S $INPUT_C_FILE -o $INTERMEDIATE_LL_FILE -fPIC -DDEBUG

# Step 2: Apply LLVM pass to generate transformed `.ll`
$LLVM_BIN_PATH/opt -S  -mode=$MODE -passes=hpsc-cfa $INTERMEDIATE_LL_FILE -o $TRANSFORMED_LL_FILE

# Step 3: Generate executables
clang $INTERMEDIATE_LL_FILE -o $EXECUTABLE_FILE
clang $TRANSFORMED_LL_FILE -o $TRANSFORMED_EXECUTABLE_FILE 

# Step 4: Move graph.dot to artifacts directory if it exists
if [ -f graph.dot ]; then
    mv graph.dot $ARTIFACTS_DIR
    dot -Tpng $ARTIFACTS_DIR/graph.dot > $ARTIFACTS_DIR/graph.png
fi

echo "X86 executables generated successfully: $EXECUTABLE_FILE and $TRANSFORMED_EXECUTABLE_FILE"
