# 838e Final

### Output asm file
```
make test/test.s
```

### Simulator
- Run `make rvsim`
- `rvsim <asm file> <max number of instructions (optional)>`

### Test Cases
- `test1.s`: Adds 2 and 3, returns `5`
- `test2.s`: Checks if 2 == 2, returns `#t`
- `test3.s`: Returns a vector of 5 cons (1. 2)
- `test4.s`: Returns a box containing 4
- `test5.s`: Returns a string "aaa"
- `test6.s`: Incorrect make-string syntax, returns `err`
- `test7.s`: Lambda adds 5 and 10, returns `15`

### RISC-V Manual
https://www.cs.sfu.ca/~ashriram/Courses/CS295/assets/notebooks/RISCV/RISCV_CARD.pdf

### Notes
- Entry point is in `compile-stdin.rkt`
- asm boilerplate is in `printer.rkt`
- Files to modify:
    - assert.rkt
    - compile-ops.rkt
    - compile.rkt
    - printer.rkt
- Register remappings:
    ```
    x86     RISC-V
    --------------
    rax ... t0
    r8  ... t1
    r9  ... t2
    r15 ... t3
    rdi ... a0      arg
    rbp ... s0      frame pointer
    rbx ... s1      heap
    rsp ... sp      stack pointer
        ... ra      link register
    ```
- Final return value is placed in `rax` / `t0`

### Modifications
- Decouple `ast/printer.rkt` dependency into local `printer.rkt`
- Removed files relating to interpreter and garbage collector
- Edited `printer.rkt` to generate RISC-V assembly code