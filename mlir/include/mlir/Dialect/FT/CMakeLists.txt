set(LLVM_TARGET_DEFINITIONS ${LLVM_MAIN_INCLUDE_DIR}/llvm/Frontend/FT/FT.td)
mlir_tablegen(FTCommon.td --gen-directive-decl --directives-dialect=FT)
add_public_tablegen_target(ft_common_td)
