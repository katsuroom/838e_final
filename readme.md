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
- Expected test outputs:
    - test.rkt: 2
    - tri.rkt: 666

### Modifications
- Decouple `ast/printer.rkt` dependency into local `printer.rkt`
- Removed files relating to interpreter and garbage collector