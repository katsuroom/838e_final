# RISC-V Simualtor

### Add Instruction
- Add new Opcode in `types.h`
- Add OpcodePair in `types.c`
- Add case in `makeInstruction()` in `parser.c`
    - Handle pseudoinstructions here
- Add case in `simulate()` in `pc.c`

### Add register
- Add new register in `types.h`
- Add RegisterPair in `types.c`
- Increase size of regfile in `pc.c`