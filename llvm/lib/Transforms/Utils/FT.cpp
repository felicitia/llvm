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

/* AutoOptimizationLevel:
      0: store instruction only, put __ft_votenow() where the instruction is
      1: both load/store instruction, put __ft_votenow() where the instruction is
      2: store instruction only, put __ft_votenow() only when the dependent auto() instruction has problem in voting.
      3: both load/store instruction only, put __ft_votenow() only when the dependent auto() instruction has problem in voting.
   + 0x10 : debugging mode
*/
#define MAX_OPT_LEVEL 3
#define MIN_OPT_LEVEL 0
static cl::opt<int>
AutoOptimizationLevel("ft-auto-optimization-level", cl::init(0), cl::Hidden,
  cl::desc("Optimization level of FT auto clause: 0 and 1"));

static bool ft_debug = false;
static void ftAuto(Function &F, int mode) ;
static void removeDuplicatedVote(Function &F) ;
static void remove_instr(Instruction &I) { };
static bool redundant_instr(Instruction & _I, Function & _F, bool is_rhs);
static void test(Module &M, ModuleAnalysisManager &AM);
static SmallVector<const BasicBlock *, 4> BBVisitedHas, BBVisited;

/*
   bit layout
   [0..3]:  (bit3:0 - LHS(0x1), RHS(0x2), BOTH(0x3)) 
   [3]:  if 1: region
   [4]:  ATOMIC
   [5]:  VOTENOW
   [6]:  AUTO
   [7]:  AUTO_REGION (START_REGION(0x0), END_REGION(0x1))
*/
#define FT_LHS 0x1
#define FT_RHS (0x1 << 1)
#define FT_BOTH_SIDES (FT_LHS | FT_RHS)
#define FT_ATOMIC (0x1 << 3)
#define FT_VOTENOW (0x1 << 4)
#define FT_AUTO (0x1 << 5) 
#define FT_AUTO_REGION_START (0x1 << 6)
#define FT_AUTO_REGION_END   (0x1 << 7)
#define FT_AUTO_REGION (FT_AUTO_REGION_START | FT_AUTO_REGION_END) 
#define FT_MASK_LHS FT_LHS
#define FT_MASK_RHS FT_RHS
#define FT_MASK_BOTH_SIDES FT_BOTH_SIDES
#define FT_MASK_ATOMIC FT_ATOMIC
#define FT_MASK_AUTO FT_AUTO
#define FT_MASK_AUTO_REGION_START FT_AUTO_REGION_START
#define FT_MASK_AUTO_REGION_END   FT_AUTO_REGION_END
#define FT_MASK_AUTO_REGION (FT_AUTO_REGION)
#define FT_MASK_VOTENOW FT_VOTENOW
#define FT_MASK_ANY (FT_MASK_LHS | FT_MASK_RHS | FT_MASK_BOTH_SIDES | FT_MASK_ATOMIC | FT_MASK_VOTENOW | FT_MASK_AUTO | FT_MASK_AUTO_REGION)

/*
   getFTInstr: returns nonzero mode of FT instruction
	ftmask: mask for lhs, rhs, atomic, auto, votnow
*/
uint32_t getFTInstr(const Instruction *I, uint32_t ftmask, Value ** arg) {
  uint32_t mode = 0;
  auto *CI = dyn_cast<CallInst>(I);
  if (CI == nullptr) return mode;
  if (!CI->getCalledFunction()) return mode;
  if (arg != nullptr) * arg = nullptr;
  llvm::StringRef name = CI->getCalledFunction()->getName();
  if ((name.find("__ft_") != std::string::npos) && (name.find("_vote") != std::string::npos)) {
    if (name.find("_auto") != std::string::npos) mode = mode | FT_AUTO;
    if (name.find("_atomic") != std::string::npos) mode = mode | FT_ATOMIC;
    if (name.find("_votel") != std::string::npos) mode = mode | FT_LHS;
    if (name.find("_voter") != std::string::npos) mode = mode | FT_RHS;
    if (name.find("_votenow") != std::string::npos) mode = FT_VOTENOW;
  } 
  if (name.find("__ft_auto") != std::string::npos) {	// exact value
    if (name.find("_auto_start") != std::string::npos)  mode = mode | FT_AUTO_REGION_START;
    else if (name.find("_auto_end") != std::string::npos) mode = mode | FT_AUTO_REGION_END;
    else {}
  }
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

static void printDL(DataDependenceGraph::DependenceList &Dependences) {
  for (auto && Dependence : Dependences) {
    llvm::errs() << "Destination Instruction: " << *Dependence->getDst() << "\n";
    std::string deptype;
    if (Dependence->isInput()) deptype = "input";
    else if (Dependence->isOutput()) deptype = "output";
    else if (Dependence->isFlow()) deptype = "flow";
    else if (Dependence->isAnti()) deptype = "anti";
    else deptype = "none";
    llvm::errs() << "Dependency Type: " << deptype << "\n";
  }
}

static bool isWithinAutoScope(int optLevel, Instruction *inst, Instruction *vinst, llvm::DataDependenceGraph &DG, DominatorTree &DT, SmallVector<Instruction *, 2> &AutoRangeStartI, SmallVector<Instruction *, 2> &AutoRangeEndI) {

  // Initialize AutoRangeStartI and AutoRangeEndI vectors if not initialized.
  if (AutoRangeStartI.size() == 0) {
    for (DDGNode *N : DG) {
      if (!isa<SimpleDDGNode>(N)) continue;
      for (Instruction *I : cast<const SimpleDDGNode>(*N).getInstructions()) {
        uint32_t mode = getFTInstr(I, FT_MASK_ANY, nullptr);
        if (mode & FT_MASK_AUTO_REGION_START) AutoRangeStartI.push_back(I);
        if (mode & FT_MASK_AUTO_REGION_END) AutoRangeEndI.push_back(I);
      }
    }
  }

  // Scope is enforced regardless of optLevel
  // if (optLevel == 1) return true;
  for (auto I : AutoRangeStartI) {
    if ((DT.dominates(I, inst) ^ DT.dominates(I, vinst)) == 1) return false;
  }
  for (auto I : AutoRangeEndI) {
    if ((DT.dominates(inst, I) ^ DT.dominates(vinst, I)) == 1) return false;
  }
  return true;
}

/* TODO: check if the pointer is the pointer of the pointer */
static bool isDependInstr(Instruction *inst, Instruction *sinst, DominatorTree &DT) {
  // for now, store instruction only
#if 0
  if (!isa<StoreInst>(inst)) return false;
#else
  if (!isa<StoreInst>(inst) && !isa<LoadInst>(inst)) return false;
#endif
  if (DT.dominates(inst, sinst)) return true;
  return false;
}

static Instruction * addVoteInstr(Instruction *inst, Instruction *vcallInst) {
  LLVMContext &ctx = inst->getContext();
  IRBuilder<> Builder(ctx);
  Instruction * nInst;

  StoreInst * sInst = dyn_cast<StoreInst>(inst);
  LoadInst * lInst = dyn_cast<LoadInst>(inst);
  Value *storedPtr;
  Value *storedValue;
  Type *valueType;
  if (sInst) {
    storedPtr = sInst->getPointerOperand();
    storedValue = sInst->getValueOperand();
    valueType = storedValue->getType();
    nInst = inst->getNextNode();
  } else if (lInst) {
    storedPtr = lInst->getPointerOperand();
    valueType = lInst->getType();
    nInst = inst;
  } else 
    return nullptr;

  if (AutoOptimizationLevel == 0 || AutoOptimizationLevel == 2)  { // only 'store' instruction
    if (sInst == nullptr) return nullptr;
  }

  if (AutoOptimizationLevel >= 2) 
    nInst = vcallInst->getNextNode();
  assert(nInst != nullptr);
  Builder.SetInsertPoint(nInst);

  Module &module = *(inst->getParent()->getParent()->getParent());
  const DataLayout &dataLayout = module.getDataLayout();

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
  
  Value * dptr;
  uint32_t mode = getFTInstr(nInst, FT_MASK_ANY, &dptr);
  if (mode == 0) return false;
  if (((mode & FT_MASK_AUTO) && !(mode & FT_MASK_AUTO_REGION)) 
     || (mode & FT_MASK_VOTENOW) || (mode & FT_MASK_BOTH_SIDES)) {
        if (ptr == dptr)
          return true;
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
    Value * vValue;
    uint32_t mode = getFTInstr(nInst, FT_MASK_ANY, &vValue);
    if (mode == 0) continue;
    if ((mode & FT_MASK_AUTO) && !(mode & FT_MASK_AUTO_REGION)) {
      Value * sValue = SI->getPointerOperand();	// stored pointer
      if (sValue == vValue)
        return nInst;
    }
  }
  return nullptr;
}

static void run_test(Function &F, FunctionAnalysisManager &AM);

PreservedAnalyses FTPass::run(Function &F,
                                      FunctionAnalysisManager &AM) {
  auto &DI = AM.getResult<DependenceAnalysis>(F);
  auto &DT = AM.getResult<DominatorTreeAnalysis>(F);

//  run_test(F, AM);
//  llvm::errs() << "++++++++++++++++++++++++++++++++++\n";
//  llvm::errs() << "++++++++++++++++++++++++++++++++++\n";

  llvm::DataDependenceGraph DG(F,DI);
  bool preserved = true;
  SmallVector<Instruction *, 2> AutoRangeStartI, AutoRangeEndI;

  if (AutoOptimizationLevel & 0x10) ft_debug = true;
  AutoOptimizationLevel = AutoOptimizationLevel & 0xf;

  if (AutoOptimizationLevel > MAX_OPT_LEVEL || AutoOptimizationLevel < MIN_OPT_LEVEL) {
    llvm::errs() << "Invalid auto-optimization-level! Forced to " << MIN_OPT_LEVEL << ".\n";
    AutoOptimizationLevel = MIN_OPT_LEVEL;
  }

  for (DDGNode *N : DG) {
    if (!isa<SimpleDDGNode>(N)) continue;
    // look for __ft_vote() call
    Instruction * vcallInst = nullptr;	// vote call inst
    Instruction * nInst = nullptr;	// new inst
    int newInstCount = 0;
    for (Instruction *I : cast<const SimpleDDGNode>(*N).getInstructions()) {
      vcallInst = isVotedAutoL(I);
      if (vcallInst == nullptr) continue;	// for voted instruction only
      Instruction * autoStartI, * autoEndI;
      LLVMContext &ctx = I->getContext();
      IRBuilder<> Builder(ctx);
      Instruction * insertInst = vcallInst;
      if (ft_debug) {
        llvm::errs() << "==================================\n";
        llvm::errs() << "Ref Instruction: " << *I << "\n";
      }
      for (auto *E : *N) { // look for memory edge, and 'store' instruction
        DataDependenceGraph::DependenceList DL;
        DG.getDependencies(*N, E->getTargetNode(), DL);
        if (ft_debug) {
          llvm::errs() << ".................................\n";
          llvm::errs() << "Edges: " << *E << "\n";
          printDL(DL);
        }
        if (E->isMemoryDependence()) {	// memory dependence only
          for (auto && Dependence : DL) {
#if 0
            if (!Dependence->isOutput()) continue;
#else
            if (!Dependence->isOutput() && !Dependence->isFlow()) continue;
#endif
            Instruction * dInst = Dependence->getDst();
            if (isVoted(dInst)) continue;
            if (!isDependInstr(dInst, I, DT)) continue;
            // if (AutoOptimizationLevel > 0 && !isWithinAutoScope(AutoOptimizationLevel, dInst, vcallInst, DG, DT, AutoRangeStartI, AutoRangeEndI)) continue;
            if (!isWithinAutoScope(AutoOptimizationLevel, dInst, vcallInst, DG, DT, AutoRangeStartI, AutoRangeEndI)) continue;
            nInst = addVoteInstr(dInst, insertInst);	// add first
            if (nInst == nullptr) continue;
            insertInst = nInst;
            preserved = false;
            newInstCount++;
//            llvm::errs() << "---> added \n";
          }
        } 
        if (ft_debug)
          llvm::errs() << "----------------------------------\n";
      }
      if (AutoOptimizationLevel >= 2 && newInstCount > 0) {	// 
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
  if (ft_debug) {
    if (!preserved)
      F.print(llvm::outs()); 
  } else {
      for (auto *I : AutoRangeStartI) I->eraseFromParent(); 
      for (auto *I : AutoRangeEndI)   I->eraseFromParent(); 
  }

  return preserved ? PreservedAnalyses::all() : PreservedAnalyses();
}

// PreservedAnalyses FTPass::run_test(Function &F, FunctionAnalysisManager &AM) {
static void run_test(Function &F, FunctionAnalysisManager &AM) {
  auto &DI = AM.getResult<DependenceAnalysis>(F);
  llvm::DataDependenceGraph DG(F,DI);

  for (DDGNode *N : DG) {
    if (isa<SimpleDDGNode>(N)) {
      llvm::errs() << "==================================\n";
      llvm::errs() << "Ref Instruction: " << *N << "\n";
          for (auto *E : *N) {
              llvm::errs() << ".................................\n";
              llvm::errs() << "Edges: " << *E << "\n";
              DataDependenceGraph::DependenceList DL;
              DG.getDependencies(*N, E->getTargetNode(), DL);
              printDL(DL);
              llvm::errs() << "----------------------------------\n";
          }
    }
  }
}


static bool redundant_instr(Instruction & _I, Function & _F, bool is_rhs) {
    for(auto & BB : _F) {
        for (auto & I: BB) {
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
      Instruction *ftI = nullptr;
      for (Use &def: arg->uses()) {
        ftI = dyn_cast<Instruction>(def);
        break;
      }
      if (ftChain.find(ftI) == ftChain.end()) {	// key does not exist
        ftChain[ftI] = SmallVector<Instruction*, 4>();
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
        for(auto & BB : F) {
            for (auto & I: BB) {
                is_lhs = is_rhs = false;
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
            }
        }
}
