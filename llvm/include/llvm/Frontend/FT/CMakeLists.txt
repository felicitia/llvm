set(LLVM_TARGET_DEFINITIONS FT.td)
tablegen(LLVM FT.h.inc --gen-directive-decl)
tablegen(LLVM FT.inc --gen-directive-impl)
add_public_tablegen_target(ft_gen)
