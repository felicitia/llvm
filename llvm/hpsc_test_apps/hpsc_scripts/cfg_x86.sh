#!/bin/bash

# Variables
INPUT_C_FILE="/home/yixue/isi-llvm/llvm/hpsc_test_apps/cfa_loop_demo.c"
OPTIMIZATION_LEVEL="O1"
BASE_NAME=$(basename $INPUT_C_FILE .c)
ARTIFACTS_DIR=$(dirname $INPUT_C_FILE)/${BASE_NAME}_x86
INTERMEDIATE_LL_FILE="$ARTIFACTS_DIR/${BASE_NAME}_${OPTIMIZATION_LEVEL}_x86.ll"
TRANSFORMED_LL_FILE="$ARTIFACTS_DIR/${BASE_NAME}_transformed_x86.ll"

# Paths
LLVM_BIN_PATH="/home/yixue/isi-llvm/build/bin"

# Generate control flow graph for the intermediate LLVM file
echo "Generating CFG for the intermediate LLVM file..."
$LLVM_BIN_PATH/opt -disable-output $INTERMEDIATE_LL_FILE -passes=dot-cfg
dot -Tpng .main.dot > $ARTIFACTS_DIR/cfg.png
echo "CFG for the intermediate LLVM file generated at $ARTIFACTS_DIR/cfg.png"

# Generate control flow graph for the transformed LLVM file
echo "Generating CFG for the transformed LLVM file..."
$LLVM_BIN_PATH/opt -disable-output $TRANSFORMED_LL_FILE -passes=dot-cfg
dot -Tpng .main.dot > $ARTIFACTS_DIR/cfg_transformed.png
echo "CFG for the transformed LLVM file generated at $ARTIFACTS_DIR/cfg_transformed.png"