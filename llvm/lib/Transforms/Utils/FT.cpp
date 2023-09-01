
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/DDG.h"
#include "llvm/Analysis/DependenceAnalysis.h"
#include "llvm/Analysis/DependenceGraphBuilder.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"

#include "llvm/Analysis/DomTreeUpdater.h"
#include "llvm/Transforms/Utils/FT.h"
#include <map>

using namespace llvm;

static cl::opt<int>
AutoOptimizationLevel("ft-auto-optimization-level", cl::init(0), cl::Hidden,
  cl::desc("Optimization level of FT auto clause: 0 and 1"));

static void ftAuto(Function &F, int mode) ;
static void ftSimple(Function &F);
static void removeDuplicatedVote(Function &F) ;
static void remove_instr(Instruction &I) { };
static bool redundant_instr(Instruction & _I, Function & _F, bool is_rhs);
static void test(Module &M, ModuleAnalysisManager &AM);
static SmallVector<const BasicBlock *, 4> BBVisitedHas, BBVisited;

std::string VoteFunctionNameR = "__ft_voter";
std::string VoteFunctionNameL = "__ft_votel";
std::string VoteFunctionNameAutoR = "__ft_voter_auto";
std::string VoteFunctionNameAutoL = "__ft_votel_auto";
std::string VoteFunctionNameV = "__ft_votenow";
std::string VoteFunctionNameAll = "__ft_voteAll";

#if 0
void getAnalysisUsage(llvm::AnalysisUsage& AU) const override {
  AU.addRequired<llvm::DependenceAnalysis>();  // Register DependenceAnalysis pass
  AU.setPreservesAll();
}
#endif

#define FT_LHS 0x1
#define FT_RHS 0x2
#define FT_BOTH_SIDES 0x3
#define FT_ATOMIC 0x4
#define FT_AUTO 0x8
#define FT_VOTENOW 0x10
#define FT_MASK_LHS FT_LHS
#define FT_MASK_RHS FT_RHS
#define FT_MASK_BOTH_SIDES FT_BOTH_SIDES
#define FT_MASK_ATOMIC FT_ATOMIC
#define FT_MASK_AUTO FT_AUTO
#define FT_MASK_VOTENOW FT_VOTENOW
#define FT_MASK_ANY (FT_MASK_LHS | FT_MASK_RHS | FT_MASK_BOTH_SIDES | FT_MASK_ATOMIC | FT_MASK_AUTO | FT_MASK_VOTENOW)

/*
	ftmask: mask for lhs, rhs, atomic, auto, votnow
*/
uint32_t getFTInstr(const Instruction *I, uint32_t ftmask, Value ** arg) {
  uint32_t mode = 0;
  auto *CI = dyn_cast<CallInst>(I);
  if (arg != nullptr)
    * arg = nullptr;
  if (CI == nullptr) return mode;
  if (!CI->getCalledFunction()) return mode;
  llvm::StringRef name = CI->getCalledFunction()->getName();
  if (name.find("__ft_vote") == std::string::npos) return mode;
  if (name.find("_auto") != std::string::npos) mode = mode | FT_AUTO;
  if (name.find("_atomic") != std::string::npos) mode = mode | FT_ATOMIC;
  if (name.find("_votel") != std::string::npos) mode = mode | FT_LHS;
  if (name.find("_voter") != std::string::npos) mode = mode | FT_RHS;
  if (name.find("_votenow") != std::string::npos) mode = FT_VOTENOW;
  if (mode != 0 && arg != nullptr) * arg = CI->getArgOperand(0);
  return (mode & ftmask);
}

Instruction * getDefInstr(Value *arg)
{
  Instruction * ftI = nullptr;
  for (Use &def: arg->uses()) {
    ftI = dyn_cast<Instruction>(def);
    break;
  }
  return ftI;
}

enum instrPosition { IS_NONE, IS_HEAD, IS_TAIL, IS_UNIQUE };

bool isSameFTInstr(const Instruction * I, const Instruction * I2) {
  Value * arg, * arg2;
  uint32_t ftType = getFTInstr(I, FT_MASK_ANY, &arg);
  uint32_t ftType2 = getFTInstr(I2, FT_MASK_ANY, &arg2);
  if (ftType == 0 || ftType2 == 0) return false;
  if ((arg == arg2) && (ftType == ftType2)) return true;
  return false;
}

static bool BBHasFTInstruction(const Instruction *I, const BasicBlock *curBB) {
  for (auto &BI : *curBB) {
    if (isSameFTInstr(I, &BI)) return true;
  }
  return false;
}

/* 
allBBHasFTInstruction(Instruction *I, BasicBlock *curBB, bool predecessor):
  return true if all predecessor BBs have the same FT (if predecessor)
  return true if all successor BBs have the same FT (if !predecessor)
*/
static bool allBBHasFTInstruction(const Instruction *I, const BasicBlock *curBB, bool predecessor) {
  if (curBB != nullptr) {
    if (I->getParent() == curBB) return true; /* the BB is in a self loop */
    if (llvm::is_contained(BBVisited, curBB)) {
      if (llvm::is_contained(BBVisitedHas, curBB)) return true;
      return false;
    }
    BBVisited.push_back(curBB);
    if (BBHasFTInstruction(I, curBB) == true) { 
      BBVisitedHas.push_back(curBB);
      return true;
    }
  } else {
    curBB = I->getParent();
  }
  bool result = false;
  if (predecessor) {
    for (auto BB : predecessors(curBB)) {
      if (allBBHasFTInstruction(I, BB, predecessor) == false) return false;
      result = true;
    }
  } else {
    for (auto BB : successors(curBB)) {
      if (allBBHasFTInstruction(I, BB, predecessor) == false) return false;
      result = true;
    }
  }
  if (result) BBVisitedHas.push_back(curBB);
  return result;
}

instrPosition getFTPositionInBB(const Instruction * I) {
  auto BB = I->getParent();
  int count, countI;
  count = countI = 0;
  for (auto &BI: *BB) {
    if (&BI == I) {
      countI = count;
      count++;
    } else if (isSameFTInstr(I, &BI)) {
      count++;
    }
  }
  assert(count > 0);
  if (count == 1) {
    assert(countI == 0);
    return IS_UNIQUE;
  } else {
    if (countI == 0) return IS_HEAD;
    if (countI == count - 1) return IS_TAIL;
  }
}

/* return true: if it is FT instr and {(RHS and IS_HEAD) or (LHS and IS_TAIL)}
          false: if it is not
*/
bool isHeadRHSOrTailLHSFTInstr(const Instruction *I, bool lookforHead, SmallVector<Instruction *, 4> & deletedInstr) {
  Value * arg;
  uint32_t ftType = getFTInstr(I, FT_MASK_LHS | FT_MASK_RHS, &arg);
  if (!ftType) return false;
  if (lookforHead && (ftType & FT_MASK_LHS)) return false;
  if (!lookforHead && (ftType & FT_MASK_RHS)) return false;
  if (llvm::is_contained(deletedInstr, I)) return false;	// FIXIT
  BBVisited.clear();
  BBVisitedHas.clear();
  Instruction *defI = getDefInstr(arg);
  if (defI == nullptr) return false;
  /* position within a BB */
  instrPosition positionInBB = getFTPositionInBB(I);
  if (positionInBB != IS_UNIQUE) {
    if ((lookforHead && (positionInBB != IS_HEAD)) || (!lookforHead && (positionInBB != IS_TAIL))) {
      return false;
    }
  } 
  if (lookforHead && (!allBBHasFTInstruction(I, nullptr, true))) {
    return true;
  }
  else if (!lookforHead && (!allBBHasFTInstruction(I, nullptr, false))) {
    return true;
  } else {
    return false;
  }
}

static void ftSimple(Function &F) {
  errs() << "FT: simple optimization \n";
  /* 1st step: leave the head voteR only, and leave tail VoteL only */
  SmallVector <Instruction *, 4> removableInstr;
  for (auto & BB: F) {
    for (auto &I: BB) {
      if (!getFTInstr(&I, FT_MASK_AUTO, nullptr)) continue;	// only for FT_AUTO
      if (isHeadRHSOrTailLHSFTInstr(&I, true, removableInstr) == true) { // HEAD && RHS
        continue;
      } else if (isHeadRHSOrTailLHSFTInstr(&I, false, removableInstr) == true) { // TAIL && LHS
        continue;
      } 
      removableInstr.push_back(&I);
    }
  }
  /* 2nd step: if voteR and voteL of the same variable having no vote* between them, it voteR is removable */
  /* Actually remove those instructions */
  for (auto *I: removableInstr) {
    llvm::errs() << "deleted : " << *I << "\n";
    I->eraseFromParent();
  }
}

static void printDL(DataDependenceGraph::DependenceList &Dependences) {
  for (auto && Dependence : Dependences) {
//    llvm::errs() << "Source Instruction: " << *Dependence->getSrc() << "\n";
    llvm::errs() << "Destination Instruction: " << *Dependence->getDst() << "\n";
    std::string deptype;
    if (Dependence->isInput()) deptype = "input";
    else if (Dependence->isOutput()) deptype = "output";
    else if (Dependence->isFlow()) deptype = "flow";
    else if (Dependence->isAnti()) deptype = "anti";
    else deptype = "none";
    llvm::errs() << "Dependency Type: " << deptype << "\n";
//    llvm::errs() << "----------------------------------\n";
  }
#if 0
  for (auto && Dependence : Dependences) {
    llvm::errs() << "Source Instruction: " << *Dependence->getSrc() << "\n";
//    llvm::errs() << "Destination Instruction: " << *Dependence->getDst() << "\n";
    std::string deptype;
    if (Dependence->isInput()) deptype = "input";
    else if (Dependence->isOutput()) deptype = "output";
    else if (Dependence->isFlow()) deptype = "flow";
    else if (Dependence->isAnti()) deptype = "anti";
    else deptype = "none";
    llvm::errs() << "Dependency Type: " << deptype << "\n";
//    llvm::errs() << "----------------------------------\n";
  }
#endif
}

/* TODO: check if the pointer is the pointer of the pointer */
static bool isDependInstr(Instruction *inst, Instruction *sinst, DominatorTree &DT) {
  // for now, store instruction only
  if (!isa<StoreInst>(inst)) return false;
  if (DT.dominates(inst, sinst)) return true;
  return false;
#if 0
  StoreInst * sInst = dyn_cast<StoreInst>(inst);
  Value *storedValue = sInst->getValueOperand();
  Type *valueType = storedValue->getType();
  if (valueType->isPointerTy()) return false;
#endif
  return true;
}

static Instruction * addVoteInstrAfter(Instruction *inst, Instruction *vcallInst) {
  LLVMContext &ctx = inst->getContext();
  IRBuilder<> Builder(ctx);
  Instruction * nInst = inst->getNextNode();
  if (AutoOptimizationLevel > 0) 
    nInst = vcallInst->getNextNode();
  assert(nInst != nullptr);
  Builder.SetInsertPoint(nInst);

  StoreInst * sInst = dyn_cast<StoreInst>(inst);
  Value *storedPtr = sInst->getPointerOperand();
  Value *storedValue = sInst->getValueOperand();
  Module &module = *(sInst->getParent()->getParent()->getParent());
  const DataLayout &dataLayout = module.getDataLayout();
  Type *valueType = storedValue->getType();

  uint64_t sizeInBytes = dataLayout.getTypeAllocSize(valueType);
  llvm::Value *TSize = llvm::ConstantInt::get(Builder.getInt32Ty(), sizeInBytes);

  llvm::Type *Params[] = {storedPtr->getType(), Builder.getInt32Ty()};
  llvm::Value *Args[] = {
    storedPtr,
    Builder.CreateIntCast(TSize, Builder.getInt32Ty(), /*isSigned*/ true)
  };
  auto *FTy = llvm::FunctionType::get(Builder.getInt32Ty(), Params, /*isVarArg=*/false);
  const char *LibCallName = "__ft_votenow";
  llvm::FunctionCallee Func = module.getOrInsertFunction(LibCallName, FTy);
  nInst = Builder.CreateCall(Func, Args, "");

  return nInst;
}

//
// isVoted (target store instruction, auto vote instruction)
//   true: if the variable of store instruction is already voted
//   false: not voted yet.
//
static bool isVoted(Instruction *inst) {
  Instruction * nInst = inst->getNextNode();	/* FIX: assume that vote is next instruction if any */
  auto *SI = dyn_cast<StoreInst>(inst);
  auto *LI = dyn_cast<LoadInst>(inst);
  if (SI == nullptr && LI == nullptr) return false;
  Value *ptr = (SI ? SI->getPointerOperand() : LI->getPointerOperand());
  if (nInst == nullptr) return false;
  if (CallInst *callInst = dyn_cast<CallInst>(nInst)) {
    Function *calledFunction = callInst->getCalledFunction();
    if (calledFunction) {
      if (calledFunction->getName() == "__ft_votel_auto" 
          || calledFunction->getName() == "__ft_votel_auto_debug"
          || calledFunction->getName() == "__ft_votenow") {
        Value * dptr = callInst->getArgOperand(0);
        if (ptr == dptr)
          return true;
      }
    }
  }
  return false;
}

static Instruction * isVotedAutoL(Instruction *inst) {
  auto * SI = dyn_cast<StoreInst>(inst);
  if (SI == nullptr) return nullptr;
  // assume that vote instruction is in the same BB
  Instruction * nInst = inst;
  while (nInst) {
    nInst = nInst->getNextNode();
    if (nInst == nullptr) return nullptr;
    CallInst *callInst = dyn_cast<CallInst>(nInst);
    if (callInst == nullptr) continue;
    Function *calledFunction = callInst->getCalledFunction();
    if (calledFunction) {
      if (calledFunction->getName() == "__ft_votel_auto" 
          || calledFunction->getName() == "__ft_votel_auto_debug") {
        Value * sValue = SI->getPointerOperand();	// stored pointer
        Value * vValue = callInst->getArgOperand(0);	// voted pointer
        if (sValue == vValue)
          return nInst;
      }
    }
  }
  return nullptr;
}

PreservedAnalyses FTPass::run(Function &F,
                                      FunctionAnalysisManager &AM) {
  auto &DI = AM.getResult<DependenceAnalysis>(F);
  auto &DT = AM.getResult<DominatorTreeAnalysis>(F);
  llvm::DataDependenceGraph DG(F,DI);
  bool preserved = true;

  for (DDGNode *N : DG) {
    if (!isa<SimpleDDGNode>(N)) continue;
    // look for __ft_vote() call
    Instruction * vcallInst = nullptr;	// vote call inst
    Instruction * nInst = nullptr;	// new inst
    int newInstCount = 0;
    for (Instruction *I : cast<const SimpleDDGNode>(*N).getInstructions()) {
      vcallInst = isVotedAutoL(I);
      if (vcallInst == nullptr) continue;	// for voted instruction only
      LLVMContext &ctx = I->getContext();
      IRBuilder<> Builder(ctx);
      Instruction * insertInst = vcallInst;
      for (auto *E : *N) { // look for memory edge, and 'store' instruction
        if (E->isMemoryDependence()) {	// memory dependence only
          DataDependenceGraph::DependenceList DL;
          DG.getDependencies(*N, E->getTargetNode(), DL);
          for (auto && Dependence : DL) {
            if (!Dependence->isOutput()) continue;
            Instruction * dInst = Dependence->getDst();
            if (isVoted(dInst)) continue;
            if (!isDependInstr(dInst, I, DT)) continue;
            nInst = addVoteInstrAfter(dInst, insertInst);	// add first
            insertInst = nInst;
            if (nInst == nullptr) continue;
            preserved = false;
            newInstCount++;
          }
        }
      }
      if (AutoOptimizationLevel == 1 && newInstCount > 0) {	// 
        BasicBlock * CurrBBsplit, * CurrBBif;
        // split the current BB into two - original + split 
        BasicBlock *CurrBB = I->getParent();
        CurrBBsplit = CurrBB->splitBasicBlock(nInst->getNextNode(), CurrBB->getName() + ".split");
        // split the current BB into two - original + ifblock 
        CurrBBif = CurrBB->splitBasicBlock(vcallInst->getNextNode(), CurrBB->getName() + ".IfBlock");
        // now branch instruction is added
        Instruction *defaultBrInstr = vcallInst->getNextNode();
        // add if statement to the original BB 
        Builder.SetInsertPoint(defaultBrInstr);
        // Create a constant integer for comparison
        ConstantInt* cmpValue = ConstantInt::get(Builder.getInt32Ty(), -1);
        // Create an ICmp instruction for comparison
        Value* callResult = vcallInst;
        Value* comparison = Builder.CreateICmpEQ(callResult, cmpValue, "cmpResult");
        Builder.CreateCondBr(comparison, CurrBBif, CurrBBsplit);
        defaultBrInstr->eraseFromParent();	// erase br instruction generated by splitBasicBlock
      }
    }
  }

// for debugging
  if (!preserved)
    F.print(llvm::outs()); 

  return preserved ? PreservedAnalyses::all() : PreservedAnalyses();
}

// PreservedAnalyses FTPass::run_test(Function &F, FunctionAnalysisManager &AM) {
PreservedAnalyses run_test(Function &F, FunctionAnalysisManager &AM) {
//  errs() << M.getName() << "\n";
  errs() << "Start" << "\n";
/*  test(M, AM); */
  auto &DI = AM.getResult<DependenceAnalysis>(F);
  llvm::DataDependenceGraph DG(F,DI);

  for (DDGNode *N : DG) {
  //  errs() << *N << "\n";
    if (isa<SimpleDDGNode>(N)) {
      llvm::errs() << "==================================\n";
      llvm::errs() << "Ref Instruction: " << *N << "\n";
//      for (const Instruction *I : cast<const SimpleDDGNode>(*N).getInstructions()) {
//        Value * arg;
//        uint32_t ftType = getFTInstr(I, FT_MASK_AUTO, &arg);
//        if (ftType) {
          for (auto *E : *N) {
              llvm::errs() << ".................................\n";
              llvm::errs() << "Edges: " << *E << "\n";
//            if (E->isMemoryDependence()) {
              DataDependenceGraph::DependenceList DL;
              DG.getDependencies(*N, E->getTargetNode(), DL);
              printDL(DL);
              llvm::errs() << "----------------------------------\n";
//            }
          }
//        }
//      }
    }
#if 0
    SmallVector<Instruction *, 2> SrcIList;
    if (isa<SimpleDDGNode>(N)) {
      errs() << " Instructions:\n";
      for (const Instruction *I : cast<const SimpleDDGNode>(*N).getInstructions())
        errs() << *I << "\n";
    } else if (isa<PiBlockDDGNode>(N)) {
      errs() << "--- start of nodes in pi-block ---\n";
      auto &Nodes = cast<const PiBlockDDGNode>(N)->getNodes();
      unsigned Count = 0;
      for (const DDGNode *N : Nodes)
        errs() << *N << (++Count == Nodes.size() ? "" : "\n");
      errs() << "--- end of nodes in pi-block ---\n";
    } else {
      errs() << " Root Node: no instruction\n";
    }
    errs() << " Edges: \n";
    for (auto *E : *N) {
      errs() << *E << "\n";
    }
#endif
  }
  ftSimple(F);
#if 0
  removeDuplicatedVote(F);
  ftAuto(F, 0);
#endif
  return PreservedAnalyses::all();
}


static bool redundant_instr(Instruction & _I, Function & _F, bool is_rhs) {
    for(auto & BB : _F) {
        for (auto & I: BB) {
//            if (I == _I) continue;
            if (llvm::CallInst *CI = llvm::dyn_cast<llvm::CallInst>(&I)) {
                llvm::CallInst *_CI = llvm::dyn_cast<llvm::CallInst>(&_I);
                if (CI->getCalledFunction() && 
                    CI->getCalledFunction()->getName() == _CI->getCalledFunction()->getName()) {
                    if (1) { 	// the same operand(0) and size(1)
                        if (is_rhs) { 
                            // load follows
                            // check its def, if both I and _I share the same def, remove
                        }
                        else { 
                            // the instruction must follow a store
                            // check its def, if both I and _I share the same def, remove
                        }
                    }
                }
            }
        }
    }
    return true;
}

static void disp_use_def(llvm::Value * v) {
  errs() << "Use-def chain of : " << *v << " \n";
  for (Use &def: v->uses()) {
    errs() << " - " << * dyn_cast<Instruction>(def) << "\n";
  }
}

static void disp_def_use(llvm::Value * v) {
  errs() << "Def-use chain\n";
  for (User *user: v->users()) {
    errs() << " - " << * dyn_cast<Instruction>(user) << "\n";
  }
}

static void test(Module &M, ModuleAnalysisManager &AM) {
  auto &DI = AM.getResult<DependenceAnalysis>(M);
  for(auto &F : M) {
    llvm::DataDependenceGraph DG(F,DI);
  }
}

// mode: 0: least (leave the first __ft_voter, and the last __ft_votel for a variable)
//       1: smart (TBD)
//       9: most (leave as it is)
// TODO: for now only one FT pragma is assumed to be used. FIX IT!
static void ftAuto(Function &F, int mode) {
  errs() << "FT: auto()" << "\n";
  if (mode == 9) return;	// do nothing
  std::map <Instruction *, SmallVector<Instruction *, 4>> ftChain;
  for(auto & BB : F) {
    for (auto & I: BB) {
      Value * arg;
      uint32_t ftType = getFTInstr(&I, FT_MASK_AUTO, &arg);
      if (ftType == 0) continue;	// non-ft function, non-ft_auto function
/*      auto *CI = dyn_cast<CallInst>(&I);
      Value *arg = CI->getArgOperand(0); */
      Instruction *ftI = nullptr;
      for (Use &def: arg->uses()) {
        ftI = dyn_cast<Instruction>(def);
        break;
      }
      // add (ft_vote* Instruction, its definition)
      if (ftChain.find(ftI) == ftChain.end()) {	// key does not exist
        ftChain[ftI] = SmallVector<Instruction*, 4>();
        // ftChain.emplace(ftI, SmallVector<Instruction*, 4>());
      }
      ftChain[ftI].push_back(&I); 
    }
  }
  errs() << "Chains of FT routines \n";
  for ( const auto &myPair : ftChain) {
    errs() << "key: " << * myPair.first << "\n ------------ \n";
    for (const auto I : myPair.second) 
      errs() << * I << "\n";
  }
}

static void removeDuplicatedVote(Function &F) {
    errs() << "FT: call removeDuplicatedVote()" << "\n";
    bool is_lhs, is_rhs;
//    for(auto &F : M) {
        for(auto & BB : F) {
            for (auto & I: BB) {
                is_lhs = is_rhs = false;
#if 1                
                Value * arg;
                uint32_t ftType = getFTInstr(&I, FT_MASK_ANY, &arg);
                if (ftType) {
                  errs() << "FT Instruction: " << I << "\n";
                  disp_use_def(arg);
                  disp_def_use(arg);
                }
                else if (auto * LI = dyn_cast<LoadInst>(&I)) {
			Value *src = LI->getPointerOperand();
                        errs() << " Instruction: " << I << "\n";
			disp_use_def(src);
			disp_def_use(src);
                }
		else if (auto * SI = dyn_cast<StoreInst>(&I)) {
			Value *dest = SI->getPointerOperand();
                        errs() << " Instruction: " << I << "\n";
			disp_use_def(dest);
			disp_def_use(dest);
                }
#if 0
                if (is_rhs || is_lhs) {
                    // check if this instruction is redundant
                    if (redundant_instr(I, F, (is_rhs ? true : false))) 
                        remove_instr(I);
                }
#endif
#else
					for(Use &U:I.operands())
					{
						Value *v = U.get();
						if(dyn_cast<Instruction>(v))
						{
							errs() << "\"" << *dyn_cast<Instruction>(v) << "\"" << " -> " << "\"" << I << "\"" << ";\n";
						}
						if (v->getName() != "") {
							errs() << "\"" << v->getName() << "\"" << " -> " << "\"" << I << "\"" << ";\n";
							errs() << "\"" << v->getName() << "\"" << " [ color = red ]\n";
						}
					}
					errs() << "\n";
#endif
            }
        }
//    }
}
