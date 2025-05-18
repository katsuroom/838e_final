# 838e Final

### Output asm file
```
make test/test.s
```

### Notes
- Entry point is in `compile-stdin.rkt`
- asm boilerplate is in `printer.rkt`

### Modifications
- Decouple `ast/printer.rkt` dependency into local `printer.rkt`
- Removed files relating to interpreter and garbage collector