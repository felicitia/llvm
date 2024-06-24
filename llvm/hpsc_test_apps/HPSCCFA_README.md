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

4. Demo with no faults (specify 2 iterations and we expect -2 as the Negated userInput)
```sh
# ./cfa_loop_demo_riscv 2
The EVEN user input is 2
EVEN loop iteration: 0
EVEN loop iteration: 1
End EVEN Loop
Average execution time per loop: 0.003329 seconds
Negated userInput is -2
Total program running time: 0.025987 seconds
Bye bye! :)
```

```sh
# ./cfa_loop_demo_riscv_transformed 2
The EVEN user input is 2
EVEN loop iteration: 0
EVEN loop iteration: 1
End EVEN Loop
Average execution time per loop: 0.003370 seconds
Negated userInput is -2
Total program running time: 0.023320 seconds
Bye bye! :)
```

5. Demo with faults

**Description:** specify 2 iterations, but jump to ODD number's for loop instead. In this case, the print messages are wrong (printing ODD loop iteration, instead of EVEN loop), but the end result `userInput` is correct. This is a very interesting case since NMR alone cannot detect this error. For example, if we vote on `userInput`, all the NMR copies will be the same and cannot detect this fault.

Original Program:

```sh
# gdb ./cfa_loop_demo_riscv
(gdb) break 17  # Line 17: int loopCount = userInput (right after `printf("The ODD user input is %d\n", userInput);`)
(gdb) run 2
(gdb) set $pc=0x0000000000010854 # Jump to for loop for ODD number instead (`0x0000000000010854 <+198>:	blez	s1,0x108b4 <main+294>` Branch if Less Than or Equal to Zero)
(gdb) c
Continuing.
ODD loop iteration: 0
ODD loop iteration: 1
End ODD Loop
Average execution time per loop: 0.005546 seconds
Negated userInput is -2
Total program running time: 0.026842 seconds
Bye bye! :)
[Inferior 1 (process 519) exited normally]
```

RISCV64-Production:

```sh
# gdb ./cfa_loop_demo_riscv_transformed
(gdb) break 17  # Line 17: int loopCount = userInput (right after `printf("The ODD user input is %d\n", userInput);`)
(gdb) run 2
(gdb) set $pc=0x00000000000108e8 # Jump to for loop for ODD number instead (`   0x00000000000108e8 <+346>:	blez	s7,0x109ae <main+544>` Branch if Less Than or Equal to Zero)
(gdb) c
Continuing.
[HPSC-CFA] Error: Control flow error detected! Runtime Signature: 3

Program received signal SIGABRT, Aborted.
0x000000000001f2ba in __pthread_kill_implementation.constprop.0 ()

```

RISCV64-Debug:

```sh
(gdb) break 17  # Line 32: int loopCount = userInput (right after Line 31: printf("The ODD user input is %d\n", userInput);)
(gdb) run 2
Runtime Sig of Parent: 0
 PreComputed Sig: 1
 PreComputed Sig Diff: 0
Runtime Sig of Parent: 1
 PreComputed Sig: 3
 PreComputed Sig Diff: 2
 XOR Result (Runtime Current Sig): 3
Runtime Sig of Parent: 3
 PreComputed Sig: 4
 PreComputed Sig Diff: 7
 XOR Result (Runtime Current Sig): 4
The EVEN user input is 2

(gdb) set $pc=0x000000000001098c # Jump to for loop for ODD number instead (`0x000000000001098c <+510>:	blez	s9,0x10ab0 <main+802>` Branch if Less Than or Equal to Zero)
(gdb) c
Continuing.
Runtime Sig of Parent: 3
 PreComputed Sig: 8
 PreComputed Sig Diff: 15
 XOR Result (Runtime Current Sig): 12
[HPSC-CFA] Error: Control flow error detected! Runtime Signature: 3

Program received signal SIGABRT, Aborted.
0x000000000001f3e6 in __pthread_kill_implementation.constprop.0 ()
```

<!-- 5. Demo with faults (specify 3 iterations and we expect -3 as the Negated userInput)
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

``` -->