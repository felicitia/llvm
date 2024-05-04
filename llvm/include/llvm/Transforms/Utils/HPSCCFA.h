#ifndef LLVM_TRANSFORMS_UTILS_HPSCCFA_H
#define LLVM_TRANSFORMS_UTILS_HPSCCFA_H

#include "llvm/IR/Function.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/IRBuilder.h"
#include <map>

using namespace llvm;

class CFABBNode {
public:
  BasicBlock *node;   // Pointer to the LLVM basic block
  unsigned sig;       // Signature of this basic block
  unsigned sigDiff;   // Signature difference for control flow checking
  unsigned sigAdj;    // Signature adjuster for complex flow scenarios
  bool isBranchFanIn; // True if this node has multiple predecessors

  // Constructor to initialize the node
  CFABBNode(BasicBlock *bb);

  // Method to set the signature
  void setSignature(unsigned s);

  // Method to check and set branch fan-in
  void checkAndUpdateBranchFanIn();

};

class HPSCCFAPass : public PassInfoMixin<HPSCCFAPass> {
public:
  std::map<BasicBlock *, CFABBNode *> graph;
  GlobalVariable* RuntimeSignature;  // Runtime signature global variable
  GlobalVariable* RuntimeSignatureAdj;  // Runtime signature adjuster global variable
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
  void createErrorBlock(Function &F, IRBuilder<> &Builder);
  void setupGlobalVariables(Module &M);
  void insertSignatureChecks(Function &F, IRBuilder<> &Builder);
  void populateGraph(Function &F);
  unsigned calculateSignatureDifference(CFABBNode* pred, CFABBNode* succ);
  void updateGraphEdges(CFABBNode* node);
  void logGraphToDotFile(const std::string &filename);
  void insertComparisonInsts(CFABBNode* node, IRBuilder<>& Builder);
  void insertStoreInsts(CFABBNode* node, IRBuilder<>& Builder, Instruction* insertSpot);
  void printCurrentInsertionPoint(IRBuilder<> &Builder);
};

#endif // LLVM_TRANSFORMS_UTILS_HPSCCFA_H
