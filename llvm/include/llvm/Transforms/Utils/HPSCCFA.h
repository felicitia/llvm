#ifndef LLVM_TRANSFORMS_UTILS_HPSCCFA_H
#define LLVM_TRANSFORMS_UTILS_HPSCCFA_H

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/PassManager.h"
#include <map>
#include <set>

using namespace llvm;

class CFABBNode {
public:
  BasicBlock *node; // Pointer to the LLVM basic block
  unsigned sig;     // Signature of this basic block
  unsigned sigDiff; // Signature difference for control flow checking
  unsigned sigAdj;  // Signature adjuster for complex flow scenarios
  unsigned
      selfLoopSigAdj; // Signature adjuster for self-loop node, as sigAdj may be
                      // a different value when it has fan-in children
  bool isBranchFanIn; // True if this node has multiple predecessors
  bool isBuffer;      // Ture if this node is a buffer node
  bool isSelfLoop;    // True if the node has an edge to itself

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
  bool DEBUG_FLAG;
  BasicBlock *errorBlock;           // each function has one error block
  GlobalVariable *RuntimeSignature; // Runtime signature global variable
  GlobalVariable
      *RuntimeSignatureAdj; // Runtime signature adjuster global variable
  GlobalVariable *RuntimeSelfLoopSignatureAdj; // Runtime self-loop signature
                                               // adjuster global variable

  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
  void createErrorBlock(Function &F, IRBuilder<> &Builder);
  void setupGlobalVariables(Module &M);
  void insertSignatureChecks(Function &F, IRBuilder<> &Builder);
  void populateGraph(Function &F, IRBuilder<> &Builder);
  void calculateSignatureDifference(CFABBNode *pred, CFABBNode *succ);
  void updateGraphEdges(CFABBNode *node);
  void logGraphToDotFile(const std::string &filename);
  void insertComparisonInsts(CFABBNode *node, IRBuilder<> &Builder);
  void insertComparisonInstsForEntryBB(CFABBNode *node, IRBuilder<> &Builder);
  void insertUpdateRuntimeSigInsts(CFABBNode *node, IRBuilder<> &Builder, Instruction *insertBefore);
  void printCurrentInsertionPoint(IRBuilder<> &Builder);
  void splitBBforCFABranch(Instruction *instToSplit);
  void addBufferNodesAll(Function &F, IRBuilder<> &Builder);
  CFABBNode *addBufferNode(Function &F, IRBuilder<> &Builder, CFABBNode *pred,
                           CFABBNode *succ);
  void addBufferNodeForSelfLoop(Function &F, IRBuilder<> &Builder);
  void addBufferNodeForFanIn(Function &F, IRBuilder<> &Builder);
  void addBufferNodeToEdge(Function &F, IRBuilder<> &Builder,
                         std::set<BasicBlock *>::const_iterator it, BasicBlock *BB,
                         std::map<BasicBlock *, CFABBNode *> &graph);
  void updatePhiNodes(CFABBNode *pred, CFABBNode *buff, CFABBNode *succ);
  void updateBranchInst(CFABBNode *pred, CFABBNode *buff, CFABBNode *succ);

  /**
   * DEBUG functions to intrument code for runtime information
   * No need to use in production
   * **/
  void DEBUG_insertPrintSigCheckingInfo(CFABBNode *node, IRBuilder<> &Builder,
                                        LoadInst *currentSig,
                                        ConstantInt *expectedSig,
                                        ConstantInt *precomputedSigDiff,
                                        BinaryOperator *xorResult);
};

#endif // LLVM_TRANSFORMS_UTILS_HPSCCFA_H
