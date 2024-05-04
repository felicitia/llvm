#include "llvm/Transforms/Utils/HPSCCFA.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdlib>
#include <fstream>
#include <unordered_map>

using namespace llvm;

static unsigned NextSignature =
    1; // Global variable for unique signature generation

unsigned generateUniqueSignature(BasicBlock *BB) {
  static std::unordered_map<BasicBlock *, unsigned> signatureMap;
  if (signatureMap.find(BB) == signatureMap.end()) {
    signatureMap[BB] = NextSignature++;
  }
  return signatureMap[BB];
}

// Constructor implementation
CFABBNode::CFABBNode(BasicBlock *bb)
    : node(bb), sig(0), sigDiff(0), sigAdj(0), isBranchFanIn(false) {}

// Method to set the signature
void CFABBNode::setSignature(unsigned s) { sig = s; }

// Method to check and update branch fan-in status
void CFABBNode::checkAndUpdateBranchFanIn() {
  isBranchFanIn = (llvm::pred_size(node) > 1);
}

void HPSCCFAPass::setupGlobalVariables(Module &M) {
  LLVMContext &Context = M.getContext();
  RuntimeSignature = new GlobalVariable(
      M, Type::getInt32Ty(Context), false, GlobalVariable::InternalLinkage,
      ConstantInt::get(Type::getInt32Ty(Context), 0), "RuntimeSignature");
  RuntimeSignatureAdj = new GlobalVariable(
      M, Type::getInt32Ty(Context), false, GlobalVariable::InternalLinkage,
      ConstantInt::get(Type::getInt32Ty(Context), 0), "RuntimeSignatureAdj");
}

void HPSCCFAPass::insertComparisonInsts(CFABBNode *node, IRBuilder<> &Builder) {
  BasicBlock *BB = node->node;
  LLVMContext &Context = BB->getContext();

  Instruction *firstNonPhi =
      BB->getFirstNonPHI(); // Get the first non-PHI instruction
  if (firstNonPhi != nullptr) {
    Builder.SetInsertPoint(firstNonPhi);
  } else {
    Builder.SetInsertPoint(BB);
  }

  ConstantInt *expectedSig =
      ConstantInt::get(Type::getInt32Ty(Context), node->sig);
  ConstantInt *expectedSigDiff =
      ConstantInt::get(Type::getInt32Ty(Context), node->sigDiff);

  // Load the runtime signature from a global variable
  LoadInst *currentSig = Builder.CreateLoad(Type::getInt32Ty(Context),
                                            RuntimeSignature, "currentSig");

  Builder.SetInsertPoint(currentSig->getNextNode());
  errs() << "Before modification:\n";
  BB->dump();

  // XOR the current signature with the expected difference
  BinaryOperator *xorResult = BinaryOperator::Create(
      Instruction::BinaryOps::Xor, currentSig, expectedSigDiff,
      Twine("xorResult"));
  Builder.Insert(xorResult);

  // Compare the XOR result with the expected signature
  Value *sigMatch = Builder.CreateICmpEQ(xorResult, expectedSig, "sigMatch");

  errs() << "After modification:\n";
  BB->dump();
}

// void HPSCCFAPass::insertStoreInsts(CFABBNode *node, IRBuilder<> &Builder,
//                                    Instruction *insertSpot) {
//   LLVMContext &Context = insertSpot->getContext();
//   IntegerType *IT1 = Type::getInt32Ty(Context);

//   // Update the current signature
//   ConstantInt *currentSig = ConstantInt::get(IT1, node->sig, false);
//   StoreInst *storeCurrentSig =
//       new StoreInst(currentSig, RuntimeSignature, false, insertSpot);

//   // Update the signature adjuster
//   ConstantInt *sigAdjVal = ConstantInt::get(IT1, node->sigAdj, false);
//   StoreInst *storeSigAdj =
//       new StoreInst(sigAdjVal, RuntimeSignatureAdj, false, insertSpot);
// }

void HPSCCFAPass::insertSignatureChecks(Function &F, IRBuilder<> &Builder) {
  BasicBlock &entryBlock = F.getEntryBlock();

  for (auto &entry : graph) {

    if (entry.second->node == &entryBlock) {
      errs() << "Entry Block sig: " << entry.second->sig << "\n";
    } else {
      errs() << "NON Entry Block sid: " << entry.second->sig << "\n";
      // Insert comparison instructions for each non-entry node in the beginning
      insertComparisonInsts(entry.second, Builder); // entry.second is CFABBNode
    }
  }
}

void HPSCCFAPass::createErrorBlock(Function &F, IRBuilder<> &Builder) {
  BasicBlock *errorBlock =
      BasicBlock::Create(F.getContext(), "error_handler", &F);
  Builder.SetInsertPoint(errorBlock);
  // Insert an unreachable instruction as a placeholder
  Builder.CreateUnreachable();
}

void HPSCCFAPass::populateGraph(Function &F) {
  // Clear previous graph entries
  graph.clear();
  for (auto &BB : F) {
    errs() << "Processing Basic Block: " << BB.getName() << "\n";
    if (graph.find(&BB) == graph.end()) {
      auto node = new CFABBNode(&BB);
      graph[&BB] = node;
      // Initialize node properties
      node->checkAndUpdateBranchFanIn();
      node->setSignature(generateUniqueSignature(&BB));
    }
  }
  // Update all edges for initial sigDiff and sigAdj calculations
  for (auto &entry : graph) {
    updateGraphEdges(entry.second);
  }
}

unsigned HPSCCFAPass::calculateSignatureDifference(CFABBNode *pred,
                                                   CFABBNode *succ) {
  unsigned sigDiff = 0;

  if (succ->isBranchFanIn) {
    // Adjust the signature only if there is a branching fan-in scenario
    if (succ->sigDiff == 0) {
      // then we haven't seen any predecessors before
      sigDiff = pred->sig ^ succ->sig;
    } else {
      // we have seen a predecessor before and need to adjust the signature
      // keep sigDiff unchanged
      sigDiff = succ->sigDiff;
      // make the predecessor adjuster whatever is needed to make
      // pn->sigAdj ^ pn->sig ^ sn->sigDiff = sn->sig
      pred->sigAdj = pred->sig ^ succ->sigDiff ^ succ->sig;
    }

  } else {
    pred->sigAdj = 0; // No adjustment needed if there is no fan-in
    sigDiff = pred->sig ^ succ->sig;
  }

  return sigDiff;
}

void HPSCCFAPass::updateGraphEdges(CFABBNode *node) {
  BasicBlock *BB = node->node;
  Instruction *TI = BB->getTerminator();
  for (unsigned i = 0, NSucc = TI->getNumSuccessors(); i < NSucc; ++i) {
    BasicBlock *Succ = TI->getSuccessor(i);
    if (graph.find(Succ) == graph.end())
      continue;
    CFABBNode *succNode = graph[Succ];

    // Calculate signature differences
    succNode->sigDiff = calculateSignatureDifference(node, succNode);
  }
}

// Method to log the graph to a DOT file
void HPSCCFAPass::logGraphToDotFile(const std::string &filename) {
  std::ofstream file(
      filename, std::ios::out); // Open file for writing, overwrite if exists
  if (!file.is_open()) {
    errs() << "Error opening file: " << filename << "\n";
    return;
  }

  // Start the digraph block
  file << "digraph G {\n";

  // Iterate through all nodes
  for (auto &entry : graph) {
    CFABBNode *node = entry.second;
    file << "  \"" << node->node->getName().str() << "\" [label=\""
         << node->node->getName().str() << "\\nSig: " << node->sig
         << "\\nSigDiff: " << node->sigDiff << "\\nSigAdj: " << node->sigAdj
         << "\\nFanIn: " << node->isBranchFanIn << "\"];\n";

    // Log edges
    Instruction *TI = node->node->getTerminator();
    for (unsigned i = 0, NSucc = TI->getNumSuccessors(); i < NSucc; ++i) {
      BasicBlock *Succ = TI->getSuccessor(i);
      if (graph.find(Succ) == graph.end())
        continue;
      file << "  \"" << node->node->getName().str() << "\" -> \""
           << Succ->getName().str() << "\";\n";
    }
  }

  file << "}\n";
  file.close();
}

void HPSCCFAPass::printCurrentInsertionPoint(IRBuilder<> &Builder) {
  if (Builder.GetInsertBlock() != nullptr) {
    auto insertPoint = Builder.GetInsertPoint();
    if (insertPoint != Builder.GetInsertBlock()->end()) {
      errs() << "Current Insert Point Instruction: ";
      insertPoint->print(errs());
      errs() << "\n";
    } else {
      errs() << "Current Insert Point is at the end of the block: "
             << Builder.GetInsertBlock()->getName() << "\n";
    }
  } else {
    errs() << "No insert block set for the builder.\n";
  }
}

PreservedAnalyses HPSCCFAPass::run(Function &F, FunctionAnalysisManager &AM) {

  LLVMContext &Context = F.getContext();
  IRBuilder<> Builder(Context);

  setupGlobalVariables(*F.getParent());
  errs() << "Function name: " << F.getName() << "\n";

  populateGraph(F);

  createErrorBlock(F, Builder);

  insertSignatureChecks(F, Builder);

  // logGraphToDotFile("graph.dot");

  return PreservedAnalyses::all();
}
