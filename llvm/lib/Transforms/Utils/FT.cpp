#include "llvm/Transforms/Utils/FT.h"
#include "llvm/IR/Instructions.h"

using namespace llvm;

static void removeDuplicatedVote(Module &M) ;
static void remove_instr(Instruction &I) { };
static bool redundant_instr(Instruction & _I, Function & _F, bool is_rhs);

PreservedAnalyses FTPass::run(Module &M,
                                      ModuleAnalysisManager &AM) {
//  errs() << M.getName() << "\n";
  errs() << "Start" << "\n";
  removeDuplicatedVote(M);
  return PreservedAnalyses::all();
}

std::string VoteFunctionNameR = "__ft_voter";
std::string VoteFunctionNameL = "__ft_votel";
std::string VoteFunctionNameV = "__ft_vote";
std::string VoteFunctionNameAll = "__ft_voteAll";


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

static void removeDuplicatedVote(Module &M) {
//    PRINT_STRING("FT: call removeDuplicatedVote()");
    bool is_lhs, is_rhs;
    for(auto &F : M) {
        for(auto & BB : F) {
            for (auto & I: BB) {
                is_lhs = is_rhs = false;
                if (auto *CI = dyn_cast<CallInst>(&I)) {
                    if (CI->getCalledFunction() && 
                        CI->getCalledFunction()->getName() == VoteFunctionNameR) {
                      // Found a call to the function with the given name
                        errs() << " Instruction: " << I << "\n";
                        Value *arg = CI->getArgOperand(0);
                        errs() << "Use-def chain\n";
                        for (Use &def: arg->uses()) {
                            errs() << " - " << * dyn_cast<Instruction>(def) << "\n";
                        }
                        is_rhs = true;
                    }
                    if (CI->getCalledFunction() && 
                        CI->getCalledFunction()->getName() == VoteFunctionNameL) {
                        errs() << " Instruction: " << I << "\n";
                        Value *arg = CI->getArgOperand(0);
                        errs() << "Def-use chain\n";
                        for (User *user: arg->users()) {
                            errs() << " - " << * dyn_cast<Instruction>(user) << "\n";
                        }
                        is_lhs = true;
                    }
                }
                if (is_rhs || is_lhs) {
                    // check if this instruction is redundant
                    if (redundant_instr(I, F, (is_rhs ? true : false))) 
                        remove_instr(I);
                }
            }
        }
    }
}
