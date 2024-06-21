## HPSC CFA Demo

We will use `cfa_loop_demo.c` as an example to demo HPSC CFA feature.

1. generate riscv64 executable
    - config `/home/yixue/isi-llvm/llvm/hpsc_test_apps/hpsc_scripts/riscv_config.sh` to specify the program to compile
    - run `/home/yixue/isi-llvm/llvm/hpsc_test_apps/hpsc_scripts/generate_riscv_executables.sh` and choose option 3

2. start HPSC QEMU using buildroot (so that you have GDB debugger that we'll use for the demo)
```sh
    ./activate
    HPSC_QEMU_MSEL=0 make run_sim # Terminal 1
    make run_linux_minicom # Terminal 2
    make run_cluster0_ssh # Terminal 3 -- We'll use this window for demo
``` 

3. In your `ssh` session, mount your riscv64 executables generated from Step 1
    `sshfs -o allow_other,default_permissions yixue@10.0.2.2:/home/yixue/isi-llvm/ /mnt`

4. Demo with no faults (specify 3 iterations and we expect -3 as the Negated userInput)
```sh
# ./cfa_loop_demo_riscv 3
The ODD user input is 3
ODD loop iteration: 0
Time spent in iteration 0: 0.002721 seconds
ODD loop iteration: 1
Time spent in iteration 1: 0.001199 seconds
ODD loop iteration: 2
Time spent in iteration 2: 0.001651 seconds
End ODD Loop
Negated userInput is -3
Total program running time: 0.025497 seconds
Bye bye! :)
```

```sh
# ./cfa_loop_demo_riscv_transformed 3
The ODD user input is 3
ODD loop iteration: 0
Time spent in iteration 0: 0.003094 seconds
ODD loop iteration: 1
Time spent in iteration 1: 0.001227 seconds
ODD loop iteration: 2
Time spent in iteration 2: 0.001137 seconds
End ODD Loop
Negated userInput is -3
Total program running time: 0.024188 seconds
Bye bye! :)
```
5. Demo with faults (specify 3 iterations and we expect -3 as the Negated userInput)
Set PC value to `negw` instruction, and the CFE will be detected at the beginning of the "return" Basic Block because we check CFA in the beginning of BB (before the `negw` instruction).

```sh
    gdb ./cfa_loop_demo_riscv
    (gdb) break clock
    (gdb) run 3
    Breakpoint 1, 0x00000000000260b8 in clock () # first clock() location before the loops
    (gdb) disassemble main # find the negw instruction
    (gdb) set $pc=0x00000000000108d0
    (gdb) c # to continue
    Continuing.
    Negated userInput is 0

    Breakpoint 1, 0x00000000000260b8 in clock ()
    (gdb) c
    Continuing.
    Total program running time: -140736674.352768 seconds
    Bye bye! :)

    Program received signal SIGSEGV, Segmentation fault.
    0x0000000000025366 in __strlen_vext ()
```

```sh
    gdb ./cfa_loop_demo_riscv_transformed
    (gdb) break clock
    (gdb) run 3
    Breakpoint 1, 0x0000000000026204 in clock ()
    (gdb) disassemble main # find the negw instruction
    (gdb) set $pc=0x00000000000109da
    (gdb) c
    Continuing.
    Negated userInput is -1

    Breakpoint 1, 0x0000000000026204 in clock ()
    (gdb) c
    Continuing.
    Total program running time: 0.214207 seconds
    Bye bye! :)
    [HPSC-CFA] Error: Control flow error detected! Runtime Signature: 13

    Program received signal SIGABRT, Aborted.
    0x000000000001f2a6 in __pthread_kill_implementation.constprop.0 ()
```

6. Demo with another fault (specify 3 iterations and we expect -3 as the Negated userInput)
This time we jump to the beginning of the BB that contains `negw`, which is `lw` instruction. This will show fault earlier and abort without printing out the negated number.

```sh
    gdb ./cfa_loop_demo_riscv_transformed
    (gdb) break clock
    (gdb) run 3
    (gdb) disassemble main # find beginning of negate block's BB:  0x0000000000010a20 <+658>:	lw	a3,-1984(gp)
    (gdb) set $pc=0x0000000000010a20
    (gdb) c
    Continuing.
    [HPSC-CFA] Error: Control flow error detected! Runtime Signature: 1

    Program received signal SIGABRT, Aborted.
    0x000000000001f2a6 in __pthread_kill_implementation.constprop.0 ()

```

7. Notes: to see more debugging info (e.g., CFA signatures), use option 1 when compiling the `.c` program in Step 1.
`./home/yixue/isi-llvm/llvm/hpsc_test_apps/hpsc_scripts/generate_riscv_executables.sh` and choose option 1
You can continue the GDB session

```sh
(gdb) run 3
    The program being debugged has been started already.
    Start it from the beginning? (y or n) y
    `/mnt/llvm/hpsc_test_apps/cfa_loop_demo_riscv64/cfa_loop_demo_riscv_transformed' has changed; re-reading symbols.
    Starting program: /mnt/llvm/hpsc_test_apps/cfa_loop_demo_riscv64/cfa_loop_demo_riscv_transformed 3
    Runtime Sig of Parent: 0
    PreComputed Sig: 1
    PreComputed Sig Diff: 0
    Runtime Sig of Parent: 1
    PreComputed Sig: 3
    PreComputed Sig Diff: 2
    XOR Result (Runtime Current Sig): 3

    Breakpoint 1, 0x0000000000026334 in clock ()
    (gdb) disassemble main   # find beginning of negate block's BB: 0x0000000000010aca <+828>:	lw	a0,-1980(gp)
    (gdb) set $pc=0x0000000000010aca
    (gdb) c
    Continuing.
    Runtime Sig of Parent: 1
    PreComputed Sig: 12
    PreComputed Sig Diff: 8
    XOR Result (Runtime Current Sig): 9
    [HPSC-CFA] Error: Control flow error detected! Runtime Signature: 1

    Program received signal SIGABRT, Aborted.
    0x000000000001f3d6 in __pthread_kill_implementation.constprop.0 ()

```