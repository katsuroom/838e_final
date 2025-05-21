#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "types.h"


typedef struct OpcodePair {
    Opcode opcode;
    char* str;
} OpcodePair;

typedef struct RegisterPair {
    Register reg;
    char* str;
} RegisterPair;


OpcodePair opcodePairs[] = {
    {ADD, "add"}, {SUB, "sub"}, {XOR, "xor"}, {OR, "or"},
    {AND, "and"}, {SLL, "sll"}, {SRL, "srl"}, {SRA, "sra"},
    {SLT, "slt"},

    {ADDI, "addi"}, {XORI, "xori"}, {ORI, "ori"},
    {ANDI, "andi"}, {SLLI, "slli"}, {SRLI, "srli"}, {SRAI, "srai"},
    {SLTI, "slti"},

    {MUL, "mul"}, {DIV, "div"}, {REM, "rem"},

    {LW, "lw"}, {SW, "sw"},

    {BEQ, "beq"}, {BNE, "bne"}, {BLT, "blt"}, {BGE, "bge"},

    {JAL, "jal"},

    {LI, "li"}, {MV, "mv"}, {J, "j"},

    {0}
};

RegisterPair registerPairs[] = {
    {x0, "zero"}, {x1, "ra"}, {x2, "sp"}, {x3, "gp"},
    {x4, "tp"}, {x5, "t0"}, {x6, "t1"}, {x7, "t2"},
    {x8, "s0"}, {x9, "s1"}, {x10, "a0"}, {x11, "a1"},
    {x12, "a2"}, {x13, "a3"}, {x14, "a4"}, {x15, "a5"},
    {x16, "a6"}, {x17, "a7"}, {x18, "s2"}, {x19, "s3"},
    {x20, "s4"}, {x21, "s5"}, {x22, "s6"}, {x23, "s7"},
    {x24, "s8"}, {x25, "s9"}, {x26, "s10"}, {x27, "s11"},
    {x28, "t3"}, {x29, "t4"}, {x30, "t5"}, {x31, "t6"},

    {x0, "x0"}, {x1, "x1"}, {x2, "x2"}, {x3, "x3"},
    {x4, "x4"}, {x5, "x5"}, {x6, "x6"}, {x7, "x7"},
    {x8, "x8"}, {x9, "x9"}, {x10, "x10"}, {x11, "x11"},
    {x12, "x12"}, {x13, "x13"}, {x14, "x14"}, {x15, "x15"},
    {x16, "x16"}, {x17, "x17"}, {x18, "x18"}, {x19, "x19"},
    {x20, "x20"}, {x21, "x21"}, {x22, "x22"}, {x23, "x23"},
    {x24, "x24"}, {x25, "x25"}, {x26, "x26"}, {x27, "x27"},
    {x28, "x28"}, {x29, "x29"}, {x30, "x30"}, {x31, "x31"},

    {0}
};


Opcode strToOpcode(char* str) {

    for(int i = 0; opcodePairs[i].str != 0; ++i) {
        if(strcmp(str, opcodePairs[i].str) == 0)
            return opcodePairs[i].opcode;
    }

    // directive
    if(str[0] == '.') return DIRECTIVE;
    
    // label
    int length = strlen(str);
    if(str[length-1] == ':') return LABEL;

    return OP_ERROR;
}

Register strToRegister(char* str) {

    int length = strlen(str);
    if(str[length-1] == ',') {
        str[length-1] = 0;
    }

    for(int i = 0; registerPairs[i].str != 0; ++i) {
        if(strcmp(str, registerPairs[i].str) == 0)
            return registerPairs[i].reg;
    }

    return REG_ERROR;
}

char* opcodeToStr(Opcode opcode) {
    for(int i = 0; opcodePairs[i].str != 0; ++i) {
        if(opcodePairs[i].opcode == opcode)
            return opcodePairs[i].str;
    }

    if(opcode == DIRECTIVE)
        return "DIRECTIVE";

    if(opcode == LABEL)
        return "LABEL";

    return NULL;
}

char* registerToStr(Register reg) {
    for(int i = 0; registerPairs[i].str != 0; ++i) {
        if(registerPairs[i].reg == reg)
            return registerPairs[i].str;
    }

    return NULL;
}

void printInstruction(Instruction instr) {
    printf("<%s>\n", opcodeToStr(instr.opcode));
}