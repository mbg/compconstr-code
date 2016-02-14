/*******************************************************************************
** Compiler for the STG Language                                              **
** By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             **
*******************************************************************************/

#include "rts.h"

StgWord Stack[STACK_SIZE];

StgWord* SpVal = Stack + STACK_SIZE - 1;
StgWord* SpPtr = Stack;

StgWord* Hp = Heap;
StgWord* HLimit = Heap + HEAP_SIZE - 1;

void dump_val_stack() {
    int i = 1;

    printf("-- Dumping value stack --\n");
    printf("SpVal = 0x%p\n", SpVal);

    for(; SpVal + i < Stack + STACK_SIZE; i++)
    {
        printf("SpVal[%d] = 0x%p\n", i, SpVal[i]);
    }
}

void dump_ptr_stack() {
    int i = 1;

    for(; SpPtr - i >= Stack; i++)
    {
        printf("SpPtr[-%d] = 0x%p\n", i, SpVal[-i]);
    }
}

void print_stack_info() {
    printf("Stack address: 0x%p\n", Stack);
    printf("SpVal: 0x%p\n", SpVal);
    printf("SpPtr: 0x%p\n", SpPtr);
}

void print_info() {
    printf("-- STG state --\n");
    print_stack_info();

    printf("Heap address: 0x%p\n", Heap);
    printf("Hp: 0x%p\n\n", Hp);
}

void finished() {
    printf("\n");

    print_info();

    printf("-- Finished! --\n");
    printf("Ret: %d\n", Ret);
    exit(0);
}

int main() {
    // print some information to aid with debugging
    print_info();

    // push a continuation onto the return stack
    printf("Pushing wrapper continuation (0x%p) onto the value stack...\n", &finished);

    SpVal[0] = &finished;
    SpVal -= 1;

    // set the address of the first continuation
    CodeLabel cont = (CodeLabel)&main_entry;

    printf("First continuation is at address 0x%p. Starting evaluation...\n", cont);

    // evaluate the program
    while(TRUE) { cont = (*cont)(); }

    // we will probably never reach this point
    return 0;
}

void stack_overflow()
{
    printf("\n!!! Stack overflow !!!\n\n");
    print_stack_info();
    exit(-1);
}

void run_gc()
{
    printf("\n!!! Out of heap space !!!\n\n");
    exit(-1);
}

void blackhole_entry() {
    printf("Black hole entered.\n");
    exit(-1);
}
