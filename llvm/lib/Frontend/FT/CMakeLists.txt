add_llvm_component_library(LLVMFrontendFT
  FT.cpp

  ADDITIONAL_HEADER_DIRS
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/Frontend
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/Frontend/FT

  DEPENDS
  ft_gen
)

target_link_libraries(LLVMFrontendFT LLVMSupport)

