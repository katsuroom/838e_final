#include <stdio.h>
#include <stdlib.h>
#include "parser.h"
#include "pc.h"
#include "print.h"

int main(int argc, char** argv) {
    if(argc == 1) {
        printf("USAGE: sim <asm file>\n");
        return 0;
    }

    FILE* file = fopen(argv[1], "r");
    if(file == NULL) {
        printf("Error opening file.\n");
        return -1;
    }

    int runtime = 1000;
    if(argc >= 3) {
        runtime = atoi(argv[2]);
    }

    setup(parse(file));

    int64_t val = simulate(runtime);
    print_result(val);

    cleanup();

    fclose(file);

    printf("\ndone\n");
}