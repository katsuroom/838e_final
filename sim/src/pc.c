#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "pc.h"

#define STACK_SIZE 4096
#define HEAP_SIZE 10000

char heap[HEAP_SIZE] = {0};
char stack[STACK_SIZE] = {0};

int64_t regfile[31] = {0};
int64_t pc = 0;

// array of instructions, not null terminated
Instruction* instructions;

// table matching labels to addresses, not null terminated
Label* labels;


int numInst = 0;
int numLabels = 0;

bool branch = false;
bool isExit = false;

void printInstruction(Instruction inst, int index) {
    printf("[%d]\t%s\trd: %s, rs1: %s, rs2: %s, imm: %d, [%ld]\n",
        index,
        opcodeToStr(inst.opcode),
        registerToStr(inst.rd),
        registerToStr(inst.rs1),
        registerToStr(inst.rs2),
        inst.imm,
        (int64_t)inst.val);
}

int matchLabel(char* label) {
    for(int i = 0; i < numLabels; ++i) {
        if(strcmp(label, labels[i].name) == 0) {
            return labels[i].index;
        }
    }

    printf("Error: Unknown label '%s'.\n", label);
    exit(EXIT_FAILURE);
    // return -1;
}

void setup(Instruction** instrs) {

    // count real instructions and labels
    Instruction** iptr;

    for(iptr = instrs; *iptr != NULL; ++iptr) {
        Instruction* instr = *iptr;

        if(instr->opcode == DIRECTIVE)
            continue;
        if(instr->opcode == LABEL) {
            numLabels++;
            continue;
        }

        numInst++;
    }

    instructions = malloc(sizeof(Instruction) * numInst);
    labels = malloc(sizeof(Label) * numLabels);

    // insert instructions and labels
    int instIndex = 0;
    int labelIndex = 0;

    for(iptr = instrs; *iptr != NULL; ++iptr) {
        Instruction* instr = *iptr;

        if(instr->opcode == DIRECTIVE) {
            free(instr);
            continue;
        }
        if(instr->opcode == LABEL) {
            labels[labelIndex].name = instr->val;
            labels[labelIndex].index = instIndex;
            free(instr);

            labelIndex++;
            continue;
        }

        instructions[instIndex] = *instr;
        free(instr);
        
        instIndex++;
    }

    free(instrs);

    // match labels with index
    for(int i = 0; i < numInst; ++i) {
        if(instructions[i].val != 0) {
            int64_t index = matchLabel(instructions[i].val);
            free(instructions[i].val);
            instructions[i].val = (char*)index;
        }
    }

    // print labels
    printf("LABELS:\n");
    for(int i = 0; i < numLabels; ++i)
        printf("%s: [%d]\n", labels[i].name, labels[i].index);

    // print instructions
    printf("\nINSTRUCTIONS:\n");
    for(int i = 0; i < numInst; ++i) {
        printInstruction(instructions[i], i);
    }
}

void cleanup() {
    for(int i = 0; i < numLabels; ++i) {
        free(labels[i].name);
    }
    free(instructions);
    free(labels);
}

void setpc(int64_t val) {
    pc = val;
    branch = true;
}

void setexit() {
    isExit = true;
}

void execute(Instruction inst) {

    switch(inst.opcode) {
    case ADD:
        regfile[inst.rd] = regfile[inst.rs1] + regfile[inst.rs2];
        break;
    case SUB:
        regfile[inst.rd] = regfile[inst.rs1] - regfile[inst.rs2];
        break;
    case XOR:
        regfile[inst.rd] = regfile[inst.rs1] ^ regfile[inst.rs2];
        break;
    case OR:
        regfile[inst.rd] = regfile[inst.rs1] | regfile[inst.rs2];
        break;
    case AND:
        regfile[inst.rd] = regfile[inst.rs1] & regfile[inst.rs2];
        break;
    case MUL:
        regfile[inst.rd] = regfile[inst.rs1] * inst.imm;
        break;
    case DIV:
        regfile[inst.rd] = regfile[inst.rs1] / inst.imm;
        break;
    case REM:
        regfile[inst.rd] = regfile[inst.rs1] % inst.imm;
        break;

    case ADDI:
        regfile[inst.rd] = regfile[inst.rs1] + inst.imm;
        break;
    case XORI:
        regfile[inst.rd] = regfile[inst.rs1] ^ inst.imm;
        break;
    case ORI:
        regfile[inst.rd] = regfile[inst.rs1] | inst.imm;
        break;
    case ANDI:
        regfile[inst.rd] = regfile[inst.rs1] & inst.imm;
        break;
    case SRAI:
        regfile[inst.rd] = regfile[inst.rs1] >> inst.imm;
        break;
    case LW:
        regfile[inst.rd] = *(int64_t*)(regfile[inst.rs1] + inst.imm);
        break;
    case SW:
        *(int64_t*)(regfile[inst.rs1] + inst.imm) = regfile[inst.rd];
        break;
    case BEQ:
        if(regfile[inst.rs1] == regfile[inst.rs2])
            setpc((int64_t)inst.val);
        break;
    case BNE:
        if(regfile[inst.rs1] != regfile[inst.rs2])
            setpc((int64_t)inst.val);
        break;
    case BLT:
        if(regfile[inst.rs1] < regfile[inst.rs2])
            setpc((int64_t)inst.val);
        break;
    case BGE:
        if(regfile[inst.rs1] >= regfile[inst.rs2])
            setpc((int64_t)inst.val);
        break;

    case JAL:
        regfile[inst.rd] = pc+1;
        setpc((int64_t)inst.val);
        break;
    case JALR:
        regfile[inst.rd] = pc+1;
        setpc(regfile[inst.rs1]);
        break;
    case RET:
        setexit();
        break;
    case LA:
        regfile[inst.rd] = (int64_t)inst.val;
        break;
    
    default:
        printf("dev: Unhandled opcode: %s\n", opcodeToStr(inst.opcode));
        break;
    }

    // maintain x0
    regfile[x0] = 0;
}

int64_t simulate(int runtime) {
    int maxInst = runtime;
    int count = 0;

    // initialize stack and frame pointer
    regfile[fp] = (int64_t)(stack + STACK_SIZE);
    regfile[sp] = regfile[fp];

    // initialize heap pointer
    regfile[a0] = (int64_t)heap;

    printf("\nRunning:\n");

    // run
    while(pc < numInst && count < maxInst) {
        
        printInstruction(instructions[pc], count);
        execute(instructions[pc]);
        count++;

        if(branch)
            branch = false;
        else
            pc++;

        if(isExit == true)
            break;

    }

    return regfile[t0];
}