#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#include "parser.h"
#include "types.h"

#define BUFFER_SIZE 256
#define MAX_PARTS 4

typedef struct ListNode {
    void* data;
    struct ListNode* next;
} ListNode;

int linenum = 0;

// tokenizer
char** splitLine(char* str, int* out);
bool isWhitespace(char c);
void printParts(char** parts);

// make instruction
Instruction* makeInstruction(char** parts, int numParts);
void verifyParts(char** parts, int numParts, int expected);
void freeParts(char** parts, int numParts);
void parseMem(char* part, Register* regOut, int* immOut);

// printing
char* allocstr(char* start);
void error(char* msg);


// allocates memory
char** splitLine(char* str, int* out) {
    
    int i = 0;
    int length = strlen(str);

    int numParts = 0;
    char** parts = calloc((MAX_PARTS+1), sizeof(char*));   // +1 for null terminator
    parts[MAX_PARTS] = 0;

    while(i < length) {

        char* start;

        while(i < length && isWhitespace(str[i]) == true)
            i++;

        if(i == length)
            break;

        if(isWhitespace(str[i]) == false) {

            // assign start
            start = str + i;

            numParts++;
            if(numParts > MAX_PARTS) {
                error("too many parts.");
            }
        }

        while(i < length && isWhitespace(str[i]) == false)
            i++;

        // assign end
        str[i] = 0;
        
        // allocate string
        parts[numParts-1] = allocstr(start);
    }

    *out = numParts;
    return parts;
}

bool isWhitespace(char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == 0;
}

void printParts(char** parts) {
    for(int i = 0; parts[i] != 0; ++i) {
        printf("[%s]", parts[i]);
    }
    printf("\n");
}


// allocates memory
Instruction* makeInstruction(char** parts, int numParts) {

    Instruction* instr = calloc(1, sizeof(Instruction));

    Opcode opcode = strToOpcode(parts[0]);
    instr->opcode = opcode;

    switch(opcode) {
    case DIRECTIVE:
        verifyParts(parts, numParts, 2);
        break;
    case LABEL: {
        verifyParts(parts, numParts, 1);

        // remove colon
        int length = strlen(parts[0]);
        parts[0][length-1] = 0;

        instr->val = allocstr(parts[0]);
    } break;
    case ADD: case SUB: case XOR: case OR: case AND:
    case SLL: case SRL: case SRA: case SLT:
    case MUL: case DIV: case REM:
        verifyParts(parts, numParts, 4);
        instr->rd = strToRegister(parts[1]);
        instr->rs1 = strToRegister(parts[2]);
        instr->rs2 = strToRegister(parts[3]);
        break;
    case ADDI: case XORI: case ORI: case ANDI:
    case SLLI: case SRLI: case SRAI: case SLTI:
        verifyParts(parts, numParts, 4);
        instr->rd = strToRegister(parts[1]);
        instr->rs1 = strToRegister(parts[2]);
        instr->imm = atoi(parts[3]);
        break;
    case LW: case SW: {
        verifyParts(parts, numParts, 3);
        instr->rd = strToRegister(parts[1]);
        Register reg;
        int imm;
        parseMem(parts[2], &reg, &imm);
        instr->rs1 = reg;
        instr->imm = imm;
    } break;
    case BEQ: case BNE: case BLT: case BGE:
        verifyParts(parts, numParts, 4);
        instr->rs1 = strToRegister(parts[1]);
        instr->rs2 = strToRegister(parts[2]);
        instr->val = allocstr(parts[3]);
        break;
    case JAL:
        verifyParts(parts, numParts, 3);
        instr->rd = strToRegister(parts[1]);
        instr->val = allocstr(parts[2]);
        break;
    default:
        free(instr);
        error("dev: unhandled opcode.");
    }

    return instr;
}

void verifyParts(char** parts, int numParts, int expected) {
    if(numParts != expected)
        error("incorrect syntax.");

    // check commas separating arguments
    for(int i = 1; i < numParts - 1; ++i) {
        int length = strlen(parts[i]);
        if(parts[i][length-1] != ',')
        error("incorrect syntax.");
    }

    // check no comma on last argument
    int length = strlen(parts[numParts-1]);
    if(parts[numParts-1][length-1] == ',')
        error("incorrect syntax.");
}

void freeParts(char** parts, int numParts) {
    for(int i = 0; i < numParts+1; ++i)
        free(parts[i]);
    free(parts);
}

void parseMem(char* part, Register* regOut, int* immOut) {
    int length = strlen(part);
    char* immStart = part;
    char* regStart = NULL;

    int i = 0;
    for(true; i < length; ++i) {
        if(part[i] == '(') {
            part[i] = 0;
            *immOut = atoi(immStart);
            break;
        }
    }

    if(i == length) error("invalid memory syntax.");

    regStart = part + i + 1;
    for(true; i < length; ++i) {
        if(part[i] == ')') {
            part[i] = 0;
            *regOut = strToRegister(regStart);
            break;
        }
    }

    if(i != length-1) error("invalid memory syntax.");
}

// allocates memory
char* allocstr(char* start) {
    int length = strlen(start);

    char* str = malloc(sizeof(char) * (length+1));
    strcpy(str, start);
    return str;
}

void error(char* msg) {
    printf("Error on line %i: %s\n", linenum, msg);
    exit(EXIT_FAILURE);
}



Instruction* parseInstruction(char* str) {
    int numParts;
    char** parts = splitLine(str, &numParts);
    // printParts(parts);

    Instruction* instr = makeInstruction(parts, numParts);

    if(instr->opcode == OP_ERROR) error("unknown opcode.");
    if(instr->rd == REG_ERROR) error("unknown register.");
    if(instr->rs1 == REG_ERROR) error("unknown register.");
    if(instr->rs2 == REG_ERROR) error("unknown register.");

    freeParts(parts, numParts);

    return instr;
}

Instruction** parse(FILE* file) {
    char buffer[BUFFER_SIZE];

    ListNode* instrList = 0;
    ListNode* tail;

    int numInstr = 0;

    // make list of instructions
    while(fgets(buffer, BUFFER_SIZE, file) != NULL) {
        linenum++;

        // if all whitespace
        bool allWhitespace = true;
        for(int i = 0; buffer[i] != 0; ++i) {
            if(isWhitespace(buffer[i]) == false) {
                allWhitespace = false;
                break;
            }
        }

        if(allWhitespace) continue;

        numInstr++;
        Instruction* instr = parseInstruction(buffer);

        ListNode* node = malloc(sizeof(ListNode));
        node->data = instr;
        node->next = NULL;

        if(instrList == 0) {
            instrList = node;
            tail = node;
        }
        else {
            tail->next = node;
            tail = tail->next;
        }
    }

    // convert instruction list to array
    Instruction** instrArr = malloc(sizeof(Instruction*) * (numInstr+1));
    instrArr[numInstr] = NULL;

    ListNode* curr = instrList;
    ListNode* next = curr->next;

    for(int i = 0; i < numInstr; ++i) {
        instrArr[i] = curr->data;
        free(curr);
        curr = next;
        if(curr != NULL)
            next = curr->next;
    }

    return instrArr;
}
