#ifndef LLVM_TRANSFORMS_FT_H
#define LLVM_TRANSFORMS_FT_H

#include "llvm/IR/PassManager.h"

namespace llvm {

class FTPass : public PassInfoMixin<FTPass> {
public:
//  PreservedAnalyses run(Module &F, ModuleAnalysisManager &AM);
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);

#if 0 // legacy
void getAnalysisUsage(llvm::AnalysisUsage& AU) const override {
  AU.addRequired<llvm::DependenceAnalysis>();  // Register DependenceAnalysis pass
  AU.setPreservesAll();
}
#endif
};

} // namespace llvm

#endif // LLVM_TRANSFORMS_FT_H
