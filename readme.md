# 838e Final

### Output asm file
```
make test/test.s
```

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
    rax ... t0
    r8  ... t1
    r9  ... t2
    r15 ... t3
    rdi ... a0      arg
    rsp ... s0      frame pointer
    rbx ... s1      heap
    ```
- Expected test outputs:
    - test.rkt: 2
    - tri.rkt: 666

### Modifications
- Decouple `ast/printer.rkt` dependency into local `printer.rkt`
- Removed files relating to interpreter and garbage collector