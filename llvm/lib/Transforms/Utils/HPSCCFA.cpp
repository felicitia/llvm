#include "llvm/Transforms/Utils/HPSCCFA.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdlib>
#include <fstream>
#include <unordered_map>

using namespace llvm;

static unsigned NextSignature =
    1; // Global variable for unique signature generation

static llvm::cl::opt<int>
    Mode("mode",
         llvm::cl::desc(
             "Specify the operational mode of the pass: \n"
             "1 - Debug mode with signature checks\n"
             "2 - Debug mode without signature checks to generate graph.dot\n"
             "3 - Production mode with signature checks"),
         llvm::cl::init(1) // Default mode
    );

unsigned generateUniqueSignature(BasicBlock *BB) {
  static std::unordered_map<BasicBlock *, unsigned> signatureMap;
  if (signatureMap.find(BB) == signatureMap.end()) {
    signatureMap[BB] = NextSignature++;
  }
  return signatureMap[BB];
}

// Constructor implementation
CFABBNode::CFABBNode(BasicBlock *bb)
    : node(bb), sig(0), sigDiff(0), sigAdj(0), selfLoopSigAdj(0),
      isBranchFanIn(false), isBuffer(false), isSelfLoop(false) {}

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
  RuntimeSelfLoopSignatureAdj = new GlobalVariable(
      M, Type::getInt32Ty(Context), false, GlobalVariable::InternalLinkage,
      ConstantInt::get(Type::getInt32Ty(Context), 0),
      "RuntimeSelfLoopSignatureAdj");
}

void HPSCCFAPass::DEBUG_insertPrintSigCheckingInfo(
    CFABBNode *node, IRBuilder<> &Builder, LoadInst *currentSig,
    ConstantInt *precomputedSig, ConstantInt *precomputedSigDiff,
    BinaryOperator *xorResult) {

  BasicBlock *BB = node->node;
  LLVMContext &Context = BB->getContext();

  // Print the function name
  Function *parentFunc = BB->getParent();
  Value *funcName = Builder.CreateGlobalStringPtr(parentFunc->getName());

  // Print the current and expected signature values and the match result
  FunctionCallee printfFunc = BB->getModule()->getOrInsertFunction(
      "printf", FunctionType::get(IntegerType::getInt32Ty(Context),
                                  {Type::getInt8PtrTy(Context)}, true));
  Value *message = Builder.CreateGlobalStringPtr(
      "Function: %s\nRuntime Sig of Parent: %d\nPreComputed Sig: "
      "%d\nPreComputed Sig Diff: %d\nXOR Result (Runtime Current Sig): %d\n");
  Builder.CreateCall(printfFunc, {message, funcName, currentSig, precomputedSig,
                                  precomputedSigDiff, xorResult});
}

/**
 * LLVM IR cannot have br instruction in the middle of the BB
 * Thus we create a new BB to perform branching based on the signature match
 * */
void HPSCCFAPass::splitBBforCFABranch(Instruction *instToSplit) {

  // Create copy of instToSplit because split function invalidates it (i.e.,
  // where instToSplit points to will no longer be valid)
  Instruction *instToSplitClone = instToSplit->clone();
  instToSplitClone->insertBefore(instToSplit);
  instToSplitClone->setName(instToSplit->getName());
  // Split the current BB into two BBs: original (currBB) + split (new BB
  // newBBsplit)
  BasicBlock *currBB = instToSplit->getParent();
  BasicBlock *newBBsplit =
      currBB->splitBasicBlock(instToSplit, currBB->getName() + ".split");
  // make new terminator for the old BB
  currBB->getTerminator()->eraseFromParent();
  BranchInst::Create(newBBsplit, this->errorBlock, instToSplitClone, currBB);
  instToSplit->eraseFromParent();
}

/**
 * Insert CFA checking instructions in the beginning of the entry BB
 * The checking instruction checks whether the current RuntimeSignature value is
 * 0
 * **/
void HPSCCFAPass::insertComparisonInstsForEntryBB(CFABBNode *node,
                                                  IRBuilder<> &Builder) {
  BasicBlock *BB = node->node;
  LLVMContext &Context = BB->getContext();

  Instruction *firstNonPhi =
      BB->getFirstNonPHI(); // Get the first non-PHI instruction
  if (firstNonPhi != nullptr) {
    Builder.SetInsertPoint(firstNonPhi);
  } else {
    Builder.SetInsertPoint(BB);
  }

  ConstantInt *precomputedSig =
      ConstantInt::get(Type::getInt32Ty(Context), node->sig);
  ConstantInt *precomputedSigDiff =
      ConstantInt::get(Type::getInt32Ty(Context), node->sigDiff);

  LoadInst *currentSig = Builder.CreateLoad(Type::getInt32Ty(Context),
                                            RuntimeSignature, "currentSig");
  // ConstantInt *zero = ConstantInt::get(Type::getInt32Ty(Context), 0);
  if (DEBUG_FLAG) {
    // Print the current and expected signature values
    FunctionCallee printfFunc = BB->getModule()->getOrInsertFunction(
        "printf", FunctionType::get(IntegerType::getInt32Ty(Context),
                                    {Type::getInt8PtrTy(Context)}, true));
    Value *message = Builder.CreateGlobalStringPtr(
        "ENTRY Runtime Sig of Parent: %d\n PreComputed "
        "Sig: %d\n PreComputed Sig Diff: %d\n");
    Builder.CreateCall(
        printfFunc, {message, currentSig, precomputedSig, precomputedSigDiff});
  }

  // Value *isInitZero = Builder.CreateICmpEQ(currentSig, zero, "isInitZero");
  // Instruction *checkInitInst = dyn_cast<Instruction>(isInitZero);
  insertUpdateRuntimeSigInsts(node, Builder, BB->getTerminator());
  // splitBBforCFABranch(checkInitInst);
}

/**
 * Insert CFA checking instructions in the beginning of the BB
 * **/
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

  ConstantInt *precomputedSig =
      ConstantInt::get(Type::getInt32Ty(Context), node->sig);
  ConstantInt *precomputedSigDiff =
      ConstantInt::get(Type::getInt32Ty(Context), node->sigDiff);

  // Load the runtime signature from a global variable
  LoadInst *currentSig = Builder.CreateLoad(Type::getInt32Ty(Context),
                                            RuntimeSignature, "currentSig");

  Builder.SetInsertPoint(currentSig->getNextNode());

  BinaryOperator *xorResult;

  if (node->isBranchFanIn) {
    // Load the runtime signature from a global variable
    LoadInst *currentSigAdj = Builder.CreateLoad(
        Type::getInt32Ty(Context), RuntimeSignatureAdj, "currentSigAdj");
    BinaryOperator *adjustedSigXor =
        BinaryOperator::Create(Instruction::BinaryOps::Xor, currentSig,
                               currentSigAdj, Twine("adjustedSigXor"));
    Builder.Insert(adjustedSigXor);
    xorResult =
        BinaryOperator::Create(Instruction::BinaryOps::Xor, adjustedSigXor,
                               precomputedSigDiff, Twine("xorResult"));
  } else {

    // XOR the current signature with the expected difference
    xorResult = BinaryOperator::Create(Instruction::BinaryOps::Xor, currentSig,
                                       precomputedSigDiff, Twine("xorResult"));
  }

  Builder.Insert(xorResult);

  if (DEBUG_FLAG) {
    DEBUG_insertPrintSigCheckingInfo(node, Builder, currentSig, precomputedSig,
                                     precomputedSigDiff, xorResult);
  }

  // Compare the XOR result with the expected signature
  Value *sigMatch = Builder.CreateICmpEQ(xorResult, precomputedSig, "sigMatch");
  Instruction *sigMatchInst = dyn_cast<Instruction>(sigMatch);
  insertUpdateRuntimeSigInsts(node, Builder, sigMatchInst);
  splitBBforCFABranch(sigMatchInst);

  // errs() << "After modification:\n";
  // BB->dump();
}

/**
 * Insert runtime signature update instructions before the insertBefore
 * instruction
 * **/
void HPSCCFAPass::insertUpdateRuntimeSigInsts(CFABBNode *node,
                                              IRBuilder<> &Builder,
                                              Instruction *insertBefore) {
  BasicBlock *BB = node->node;
  LLVMContext &Context = BB->getContext();
  IntegerType *IT1 = Type::getInt32Ty(Context);

  // Set the insertion point before the specified instruction
  Builder.SetInsertPoint(insertBefore);

  // Update the current signature
  ConstantInt *precomputedSig = ConstantInt::get(IT1, node->sig, false);
  Builder.CreateStore(precomputedSig, RuntimeSignature);

  // Update the signature adjuster
  ConstantInt *precomputedSigAdj = ConstantInt::get(IT1, node->sigAdj, false);
  Builder.CreateStore(precomputedSigAdj, RuntimeSignatureAdj);
}

void HPSCCFAPass::insertSignatureChecks(Function &F, IRBuilder<> &Builder) {
  BasicBlock &entryBlock = F.getEntryBlock();

  errs() << "insert signature checking instructions...\n";
  for (auto &entry : graph) {

    if (entry.second->node == &entryBlock) {
      if (DEBUG_FLAG) {
        errs() << "Entry Block sig: " << entry.second->sig << "\n";
        errs() << "insert signature updating instructions for entry BB...\n";
      }
      insertComparisonInstsForEntryBB(entry.second,
                                      Builder); // entry.second is CFABBNode
    } else {
      if (DEBUG_FLAG) {
        errs() << "NON-Entry Block sig: " << entry.second->sig
               << "\tBB: " << entry.first->getName() << "\n";
      }
      // Insert comparison instructions for each non-entry node in the beginning
      if (DEBUG_FLAG) {
        errs() << "insert signature comparison instructions...\n";
      }
      insertComparisonInsts(entry.second, Builder); // entry.second is CFABBNode
    }
  }
}

void HPSCCFAPass::createErrorBlock(Function &F, IRBuilder<> &Builder) {
  // Create a new error handling block
  BasicBlock *errorBlock =
      BasicBlock::Create(F.getContext(), "error_handler", &F);
  this->errorBlock = errorBlock;

  // Set the insertion point to the new error block
  Builder.SetInsertPoint(errorBlock);

  // Create instructions to print an error message including the block's
  // signature
  FunctionCallee printfFunc = F.getParent()->getOrInsertFunction(
      "printf", FunctionType::get(IntegerType::getInt32Ty(F.getContext()),
                                  {Type::getInt8PtrTy(F.getContext()),
                                   IntegerType::getInt32Ty(F.getContext())},
                                  true));
  Value *errorMessage = Builder.CreateGlobalStringPtr(
      "[HPSC-CFA] Error: Control flow error detected! Runtime Signature: %d\n");

  // Assuming that RuntimeSignature is a global variable holding the signature
  LoadInst *runtimeSig = Builder.CreateLoad(
      IntegerType::getInt32Ty(F.getContext()), RuntimeSignature, "runtimeSig");
  Builder.CreateCall(printfFunc, {errorMessage, runtimeSig});

  // Insert a call to abort the program
  FunctionCallee abortFunc = F.getParent()->getOrInsertFunction(
      "abort", FunctionType::get(Type::getVoidTy(F.getContext()), false));
  Builder.CreateCall(abortFunc, {});

  // No need to continue after abort; insert an unreachable instruction
  Builder.CreateUnreachable();
}

/***
 * Add buffer nodes in the complicated cases when the sigAdj can be overwritten
 */
void HPSCCFAPass::addBufferNodesAll(Function &F, IRBuilder<> &Builder) {

  addBufferNodeForSelfLoop(F, Builder);
  std::string filename = (F.getName() + "_noSelfLoop.dot").str();
  logGraphToDotFile(filename);
  // Update isBranchFanIn field after inserting buffer node to self-loop nodes
  for (auto &BB : F) {
    auto currentNode = graph.find(&BB);
    if (currentNode != graph.end()) {
      // Node already exists, just update its properties
      currentNode->second->checkAndUpdateBranchFanIn();
    } else {
      errs() << "Error: BasicBlock " << BB.getName()
             << " not found in graph.\n";
      std::abort();
    }
  }
  filename = (F.getName() + "_beforeFanIn.dot").str();
  logGraphToDotFile(filename);
  addBufferNodeForFanIn(F, Builder);
  filename = (F.getName() + "_afterFanIn.dot").str();
  logGraphToDotFile(filename);
}

/***
 * Handle self-loop nodes by inserting a buffer node to break the self-loop.
 */
void HPSCCFAPass::addBufferNodeForSelfLoop(Function &F, IRBuilder<> &Builder) {
  for (auto &entry : graph) {
    BasicBlock *BB = entry.first;
    CFABBNode *node = entry.second;

    // Handle self-loops
    if (std::find(successors(BB).begin(), successors(BB).end(), BB) !=
        successors(BB).end()) {
      // This block is a self-loop; add a buffer node
      CFABBNode *bufferNode = addBufferNode(F, Builder, node, node);
      Instruction *TI = BB->getTerminator();
      for (unsigned i = 0, e = TI->getNumSuccessors(); i != e; ++i) {
        if (TI->getSuccessor(i) == BB) {
          TI->setSuccessor(i, bufferNode->node);
        }
      }
    }
  }
}

void HPSCCFAPass::addBufferNodeToEdge(
    Function &F, IRBuilder<> &Builder,
    std::set<BasicBlock *>::const_iterator it, BasicBlock *BB,
    std::map<BasicBlock *, CFABBNode *> &graph) {
  // Use the existing addBufferNode function to handle insertion
  CFABBNode *bufferNode = addBufferNode(F, Builder, graph[BB], graph[*it]);
  // Redirect the original block's terminator to point to the buffer
  // block
  Instruction *TI = BB->getTerminator();
  for (unsigned i = 0, e = TI->getNumSuccessors(); i != e; ++i) {
    if (TI->getSuccessor(i) == *it) {
      TI->setSuccessor(i, bufferNode->node);
    }
  }
}

/***
 * Handle fan-in problem when the parent has multiple children who are both
 * fan-in nodes. This will override signature adjuster. Solution: We insert
 * buffer node that does not need the adjuster to avoid this case.
 */
void HPSCCFAPass::addBufferNodeForFanIn(Function &F, IRBuilder<> &Builder) {
  for (auto &entry : graph) {
    BasicBlock *BB = entry.first;
    // Handle multiple fan-in nodes
    if (llvm::succ_size(BB) > 1) {
      std::set<BasicBlock *> fanInSuccs;
      std::set<BasicBlock *> fanInDuplicateSuccs;
      std::vector<BasicBlock *> fanInSuccsList;
      for (BasicBlock *Succ : successors(BB)) {
        if (graph[Succ]->isBranchFanIn) {
          if (fanInSuccs.find(Succ) != fanInSuccs.end()) {
            fanInDuplicateSuccs.insert(Succ);
          }
          fanInSuccs.insert(Succ);
          fanInSuccsList.push_back(Succ);
        }
      }

      if (fanInSuccs.size() > 1) {
        // Handle the cases where there are fan-in nodes with duplicated edges
        // of the same parent and child
        if (fanInDuplicateSuccs.size() > 0) {
          // If only one fan-in node has duplicated edges, we treat it as the
          // first node without adding buffer node, and add buffer node to the
          // rest of the fan-in nodes
          if (fanInDuplicateSuccs.size() == 1) {
            for (auto it = fanInSuccs.begin(); it != fanInSuccs.end(); ++it) {
              if (std::find(fanInDuplicateSuccs.begin(),
                            fanInDuplicateSuccs.end(),
                            *it) == fanInDuplicateSuccs.end()) {
                addBufferNodeToEdge(F, Builder, it, BB, graph);
              }
            }
          } else { // If there are multiple fan-in nodes with duplicated edges
            auto firstDuplicateToSkip = std::next(fanInDuplicateSuccs.begin());
            for (size_t i = 0; i < fanInSuccsList.size(); ++i) {
              BasicBlock *current = fanInSuccsList[i];
              // Check if current is in fanInDuplicateSuccs
              bool isInDuplicateSuccs =
                  std::find(fanInDuplicateSuccs.begin(),
                            fanInDuplicateSuccs.end(),
                            current) != fanInDuplicateSuccs.end();
              // Check if current is not the first duplicate to skip
              bool isNotFirstDuplicateToSkip =
                  std::find(firstDuplicateToSkip, fanInDuplicateSuccs.end(),
                            current) == fanInDuplicateSuccs.end();
              if (isInDuplicateSuccs && isNotFirstDuplicateToSkip) {
                auto it = std::find(fanInSuccsList.begin(),
                                    fanInSuccsList.end(), current);
                CFABBNode *bufferNode =
                    addBufferNode(F, Builder, graph[BB], graph[*it]);
                // Redirect the original block's terminator to point to the
                // buffer block
                Instruction *TI = BB->getTerminator();
                for (unsigned i = 0, e = TI->getNumSuccessors(); i != e; ++i) {
                  if (TI->getSuccessor(i) == *it) {
                    TI->setSuccessor(i, bufferNode->node);
                  }
                }
              }
            }
            // add buffer node to all the non-duplicated fan-in nodes
            for (auto it = fanInSuccs.begin(); it != fanInSuccs.end(); ++it) {
              if (std::find(fanInDuplicateSuccs.begin(),
                            fanInDuplicateSuccs.end(),
                            *it) == fanInDuplicateSuccs.end()) {
                addBufferNodeToEdge(F, Builder, it, BB, graph);
              }
            }
          }

        } else {
          // Iterate over all but the first fan-in successor to add buffer nodes
          for (auto it = std::next(fanInSuccs.begin()); it != fanInSuccs.end();
               ++it) {
            // Use the existing addBufferNode function to handle insertion
            CFABBNode *bufferNode =
                addBufferNode(F, Builder, graph[BB], graph[*it]);
            // Redirect the original block's terminator to point to the buffer
            // block
            Instruction *TI = BB->getTerminator();
            for (unsigned i = 0, e = TI->getNumSuccessors(); i != e; ++i) {
              if (TI->getSuccessor(i) == *it) {
                TI->setSuccessor(i, bufferNode->node);
              }
            }
          }
        }
      }
    }
  }
}

CFABBNode *HPSCCFAPass::addBufferNode(Function &F, IRBuilder<> &Builder,
                                      CFABBNode *pred, CFABBNode *succ) {
  errs() << "-Inserting a buffer node-\n";
  errs() << "  Between " << pred->node->getName() << " and "
         << succ->node->getName() << "\n";

  Twine name = "Buffer_" + pred->node->getName() + "_" + succ->node->getName();
  BasicBlock *bufferBB =
      BasicBlock::Create(F.getContext(), name, &F, succ->node);
  CFABBNode *CFAbuffer = new CFABBNode(bufferBB);
  graph[bufferBB] = CFAbuffer;
  CFAbuffer->isBuffer = true;

  // update the branch instruction in pred
  updateBranchInst(pred, CFAbuffer, succ);
  // make buff terminator point only to succ
  BranchInst::Create(succ->node, CFAbuffer->node);

  // don't forget to change phi node targets in succ (if any exist)
  updatePhiNodes(pred, CFAbuffer, succ);
  succ->isBranchFanIn = true;

  // returns a pointer to the new node
  return CFAbuffer;
}

void HPSCCFAPass::updateBranchInst(CFABBNode *pred, CFABBNode *buff,
                                   CFABBNode *succ) {
  // Get the terminator instruction of the predecessor block
  llvm::Instruction *TI = pred->node->getTerminator();

  // Iterate through all successors of the terminator instruction
  for (unsigned i = 0, NSucc = TI->getNumSuccessors(); i < NSucc; ++i) {
    if (TI->getSuccessor(i) == succ->node) {
      // Update the successor to point to the buffer node
      TI->setSuccessor(i, buff->node);
    }
  }
}

void HPSCCFAPass::populateGraph(Function &F, IRBuilder<> &Builder) {
  // Clear previous graph entries
  graph.clear();

  // initiate nodes in the graph
  for (auto &BB : F) {
    if (graph.find(&BB) == graph.end()) {
      auto node = new CFABBNode(&BB);
      graph[&BB] = node;
    }
  }
  std::string filename = (F.getName() + "_init.dot").str();
  logGraphToDotFile(filename);
  addBufferNodesAll(F, Builder);

  for (auto &BB : F) {
    if (DEBUG_FLAG) {
      errs() << "Adding Pre-Computed Signatures to Basic Block: "
             << BB.getName() << "\n";
    }
    auto currentNode = graph.find(&BB);
    if (currentNode != graph.end()) {
      // Node already exists, just update its properties
      CFABBNode *node = currentNode->second;
      node->setSignature(generateUniqueSignature(&BB));
    } else {
      // Handle error: Basic block not found in the graph
      errs() << "Error: Basic Block '" << BB.getName()
             << "' not found in graph.\n";
      errs() << "Aborting program.\n";
      abort();
    }
  }

  // Update all edges for sigDiff and sigAdj calculations
  for (auto &entry : graph) {
    updateGraphEdges(entry.second); // entry.second is CFABBNode
  }
}

void HPSCCFAPass::updatePhiNodes(CFABBNode *pred, CFABBNode *buff,
                                 CFABBNode *succ) {
  // This function will change the phi node predecessors in succ from pred to
  // buff
  BasicBlock *succBB = succ->node;
  for (auto &I : *succBB) {
    // Check if the instruction is a phi node
    if (PHINode *phi = dyn_cast<PHINode>(&I)) {
      // Iterate through each of the incoming edges
      int incomingIndex = phi->getBasicBlockIndex(pred->node);
      if (incomingIndex != -1) { // Check if pred is an incoming block
        // Replace the incoming block (pred) with the buffer block (buff)
        phi->setIncomingBlock(incomingIndex, buff->node);
      }
    } else {
      // Stop modifying if it's not a phi node since all phi nodes are at the
      // beginning
      break;
    }
  }
}
/***
 * update sigDiff, sigAdj, selfLoopSigAdj, isSelfLoop when iterating each edge
 * in the graph
 */
void HPSCCFAPass::calculateSignatureDifference(CFABBNode *pred,
                                               CFABBNode *succ) {

  errs() << "Precomputing  for pred sig: " << pred->sig
         << ", succ sig: " << succ->sig << "\n";

  if (pred == succ) {
    errs() << "Error: Self-loop detected at " << pred->node->getName()
           << ". Aborting.\n";
    abort();
  }

  // Adjust the signature only if there is a branching fan-in scenario
  if (succ->isBranchFanIn) {
    // It's possible when self-loop edge is visited before and the sigDiff is
    // still 0
    if (succ->sigDiff == 0 && pred->isSelfLoop == false) {
      // then this is the first predecessor of the fan-in node, we do not adjust
      // (sigAdj = 0)
      succ->sigDiff = pred->sig ^ succ->sig;
    } else {
      // update predecessor's adjuster such that
      // runtime signature 𝐺4(runtime succ sig) = 𝐺3(runtime pred
      // sig)⊕𝑑4(succ->sigDiff)⊕𝐷3(pred->sigAdj)
      if (pred == succ) {
        pred->selfLoopSigAdj = pred->sig ^ succ->sigDiff ^ succ->sig;
      } else {
        // we have seen a predecessor before and need to adjust the signature
        if (succ->sigDiff == 0) {
          succ->sigDiff = pred->sig ^ succ->sig;
        }
        pred->sigAdj = pred->sig ^ succ->sigDiff ^ succ->sig;
      }
    }

  } else {
    // pred->sigAdj =
    //     0; // No adjustment needed if there is no fan-in (no self-loop
    //     either)
    succ->sigDiff = pred->sig ^ succ->sig;
  }

  if (pred == succ) {
    errs() << "Precomputed pred selfLoopigadj is: " << pred->sigAdj
           << ", succ sigDiff is: " << succ->sigDiff << "\n";
  } else {
    errs() << "Precomputed pred sigadj is: " << pred->sigAdj
           << ", succ sigDiff is: " << succ->sigDiff << "\n";
  }
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
    calculateSignatureDifference(node, succNode);
    errs() << "\nFunction:" << BB->getParent()->getName()
           << "\nUpdating edge from " << BB->getName() << " to "
           << Succ->getName() << "\n";
    // BB->dump();
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
         << "\\nSelfLoopSigAdj: " << node->selfLoopSigAdj
         << "\\nSelfLoop: " << node->isSelfLoop
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

  switch (Mode) {
  case 1: {
    DEBUG_FLAG = true; // Turn on debug flag
    errs() << "Running in debug mode with signature checks.\n";
    populateGraph(F, Builder);
    createErrorBlock(F, Builder);
    std::string filename = (F.getName() + "_final.dot").str();
    logGraphToDotFile(filename);
    insertSignatureChecks(F,
                          Builder); // graph structure will be invalid after
                                    // inserting instructions as BB gets split
  } break;
  case 2: {
    DEBUG_FLAG = true; // Turn on debug flag
    errs() << "Running in debug mode without signature checks to generate "
              "graph.dot.\n";
    populateGraph(F, Builder);
    createErrorBlock(F, Builder);
    std::string filename = (F.getName() + "_final.dot").str();
    logGraphToDotFile(filename);
  } break;
  case 3:
    DEBUG_FLAG = false; // Turn off debug flag
    errs() << "Running in production mode with signature checks.\n";
    populateGraph(F, Builder);
    createErrorBlock(F, Builder);
    insertSignatureChecks(F,
                          Builder); // graph structure will be invalid after
                                    // inserting instructions as BB gets split
    break;
  default:
    errs() << "Invalid mode specified.\n";
    break;
  }

  return PreservedAnalyses::all();
}