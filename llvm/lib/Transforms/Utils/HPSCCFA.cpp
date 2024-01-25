#include "llvm/Transforms/Utils/HPSCCFA.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

PreservedAnalyses HPSCCFAPass::run(Function &F, FunctionAnalysisManager &AM) {

  errs() << "Function name: " << F.getName() << "\n";

  for (BasicBlock &BB : F) {
    if (!BB.getName().empty()) {
      errs() << "Basic Block: " << BB.getName() << "\n";
    } else {
      errs() << "Basic Block: (unnamed)\n";
    }
    errs() << "Predecessors: ";

    if (pred_begin(&BB) == pred_end(&BB)) {
      errs() << "None";
    } else {
      for (BasicBlock *Pred : predecessors(&BB)) {
        if (Pred->getName().empty()) {
          errs() << "(unnamed) ";
        } else {
          errs() << Pred->getName() << " ";
        }
      }
    }
    errs() << "\n";
  }

  return PreservedAnalyses::all();
}
