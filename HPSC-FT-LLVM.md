# How to build HPSC FT (Fault Tolerance) LLVM

HPSC FT LLVM is a main LLVM with a HPSC FT patch. 
You can build HPSC FT LLVM in the same way how you build an upstream LLVM.
To build HPSC FT project, you need to add `ft` to `-DLLVM_ENABLE_PROJECTS`.
Here is an example which enables HPSC FT project in addition to clang and openmp.
The following command also builds clang for three different architectures - X86, AArch64 and RISCV.

```bash
// git clone <hpsc-llvm-project repository> hpsc-llvm
$ cd hpsc-llvm
$ mkdir build
$ cd build
$ cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/tmp/llvm -DLLVM_ENABLE_PROJECTS="clang;openmp;ft" -G "Unix Makefiles" ../llvm -DLLVM_TARGETS_TO_BUILD="X86;AArch64;RISCV"
$ make -j 32
$ make install
```
