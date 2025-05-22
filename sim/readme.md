# RISC-V Simulator

### Add Instruction
- Add new Opcode in `enums.h`
- Add OpcodePair in `enums.c`
- Add case in `makeInstruction()` in `parser.c`
    - Convert pseudoinstructions here
- Add case in `simulate()` in `pc.c`
    - No need to handle pseudoinstructions

### Add register
- Add new register in `enums.h`
- Add RegisterPair in `enums.c`
- Increase size of regfile in `pc.c`