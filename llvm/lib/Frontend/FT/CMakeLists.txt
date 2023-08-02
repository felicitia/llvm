add_llvm_component_library(LLVMFrontendFT
  FT.cpp
  FTIRBuilder.cpp

  ADDITIONAL_HEADER_DIRS
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/Frontend
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/Frontend/FT

  DEPENDS
  intrinsics_gen
  ft_gen

  LINK_COMPONENTS
  Core
  Support
  TargetParser
  TransformUtils
  Analysis
  MC
  Scalar
  BitReader
)


