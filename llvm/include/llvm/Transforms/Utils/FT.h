#ifndef LLVM_TRANSFORMS_FT_H
#define LLVM_TRANSFORMS_FT_H

#include "llvm/IR/PassManager.h"

namespace llvm {

class FTPass : public PassInfoMixin<FTPass> {
public:
  PreservedAnalyses run(Module &F, ModuleAnalysisManager &AM);
};

} // namespace llvm

#endif // LLVM_TRANSFORMS_FT_H
