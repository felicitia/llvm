#!/bin/bash

source riscv_config.sh

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

# Step 1: Generate intermediate `.ll`
clang $INCLUDE_PATHS -O${OPTIMIZATION_LEVEL:1} -g -emit-llvm -S --target=riscv64 $INPUT_C_FILE -o $INTERMEDIATE_LL_FILE -mabi=lp64d -fno-addrsig -fPIC -DDEBUG

# Step 2: Apply LLVM pass to generate transformed `.ll`
$LLVM_BIN_PATH/opt -S -mode=$MODE -passes=hpsc-cfa $INTERMEDIATE_LL_FILE -o $TRANSFORMED_LL_FILE

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
    dot -Tpng $ARTIFACTS_DIR/graph.dot > $ARTIFACTS_DIR/graph.png
fi

echo "RISC-V executables generated successfully: $EXECUTABLE_FILE and $TRANSFORMED_EXECUTABLE_FILE"
