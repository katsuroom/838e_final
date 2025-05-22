#ifndef PC_H
#define PC_H

#include "enums.h"

void setup(Instruction** instrs);

int64_t simulate(int runtime);

void cleanup();

#endif