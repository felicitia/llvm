#!/bin/bash

source riscv_config.sh

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