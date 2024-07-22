#!/bin/bash

# Variables
INPUT_C_FILE="/home/yixue/isi-llvm/llvm/hpsc_test_apps/cfa_factorial_recursion.c"
OPTIMIZATION_LEVEL="O1"
BASE_NAME=$(basename $INPUT_C_FILE .c)
ARTIFACTS_DIR=$(dirname $INPUT_C_FILE)/${BASE_NAME}_x86
INTERMEDIATE_LL_FILE="$ARTIFACTS_DIR/${BASE_NAME}_${OPTIMIZATION_LEVEL}_x86.ll"
TRANSFORMED_LL_FILE="$ARTIFACTS_DIR/${BASE_NAME}_transformed_x86.ll"

# Paths
LLVM_BIN_PATH="/home/yixue/isi-llvm/build/bin"
SCRIPT_DIR=$(pwd)

# Delete existing dot files that start with "."
echo "Deleting old CFG dot files..."
rm -f $SCRIPT_DIR/.*.dot

# Generate control flow graph for the intermediate LLVM file
echo "Generating CFG for the intermediate LLVM file..."
$LLVM_BIN_PATH/opt -disable-output $INTERMEDIATE_LL_FILE -passes=dot-cfg

# Convert newly generated dot files to PNG and move them to the artifacts directory
echo "Converting dot files to PNG..."
for dotfile in $SCRIPT_DIR/.*.dot; do
    if [ -f "$dotfile" ]; then
        # Strip the leading "." from the filename for the PNG file
        filename=$(basename "$dotfile")
        filename="${filename#.}"  # Remove the leading dot
        pngfile="$ARTIFACTS_DIR/${filename%.dot}.png"  # Replace .dot with .png
        dot -Tpng "$dotfile" > "$pngfile"
        echo "CFG for function ${filename%.dot} generated at $pngfile"
    fi
done

# Generate control flow graph for the transformed LLVM file
echo "Generating CFG for the transformed LLVM file..."
$LLVM_BIN_PATH/opt -disable-output $TRANSFORMED_LL_FILE -passes=dot-cfg

echo "Converting dot files to PNG for transformed LLVM file..."
for dotfile in $SCRIPT_DIR/.*.dot; do
    if [ -f "$dotfile" ]; then
        # Strip the leading "." from the filename for the PNG file
        filename=$(basename "$dotfile")
        filename="${filename#.}"  # Remove the leading dot
        pngfile="$ARTIFACTS_DIR/${filename%.dot}_transformed.png"  # Replace .dot with .png and append "_transformed"
        dot -Tpng "$dotfile" > "$pngfile"
        echo "CFG for function ${filename%.dot} generated at $pngfile"
    fi
done

echo "CFG transformation completed."