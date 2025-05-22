#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>

typedef enum Opcode{
    NOP,
    DIRECTIVE, LABEL,
    ADD, SUB, XOR, OR, AND, SLL, SRL, SRA, SLT,
    MUL, DIV, REM,
    ADDI, XORI, ORI, ANDI, SLLI, SRLI, SRAI, SLTI,
    LW, SW,
    BEQ, BNE, BLT, BGE,
    J, JAL, JALR,

    LI, MV, RET, LA,

    OP_ERROR
} Opcode;

/*
    x0 = zero
    t0 = rax
    t1 = scratch r8
    t2 = scratch r9
    t3 = scratch r15
    s0 = frame pointer
    s1 = heap
    a0 = arg
    sp = stack pointer
*/
typedef enum Register {
    x0, x1, x2, x3, x4, x5, x6, x7,
    x8, x9, x10, x11, x12, x13, x14, x15,
    x16, x17, x18, x19, x20, x21, x22, x23,
    x24, x25, x26, x27, x28, x29, x30, x31,

    REG_ERROR
} Register;

enum RegisterAlt{
    zero, ra, sp, gp, tp, t0, t1, t2,
    s0, s1, a0, a1, a2, a3, a4, a5,
    a6, a7, s2, s3, s4, s5, s6, s7,
    s8, s9, s10, s11, t3, t4, t5, t6,
    fp = 8
};

typedef struct Instruction {
    Opcode opcode;
    Register rd, rs1, rs2;
    int32_t imm;
    char* val;
} Instruction;

typedef struct Label {
    char* name;
    int index;          // index within instructions array
} Label;


Opcode strToOpcode(char* str);

Register strToRegister(char* str);

char* opcodeToStr(Opcode opcode);

char* registerToStr(Register reg);

#endif