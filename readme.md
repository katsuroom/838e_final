# 838e Final

### Output asm file
```
make test/test.s
```

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
    rsp ... s0      frame pointer
    rbx ... s1      heap
            sp      stack pointer
    ```
- Final return value is placed in `rax` / `t0`
- Expected test outputs:
    - test.rkt: 2
    - tri.rkt: 666

### Simulator
- Run `make rvsim`
- `rvsim <asm file>`

### Modifications
- Decouple `ast/printer.rkt` dependency into local `printer.rkt`
- Removed files relating to interpreter and garbage collector