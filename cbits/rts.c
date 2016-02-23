/*******************************************************************************
** Compiler for the STG Language                                              **
** By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             **
*******************************************************************************/

#include "rts.h"

// the shared stack space
StgWord Stack[STACK_SIZE];

// initialise the value stack pointer and the pointer stack pointer
StgWord* SpVal = Stack + STACK_SIZE - 1;
StgWord* SpPtr = Stack;

// initialise the heap pointers to the A heap
StgWord* Heap = HeapA;
StgWord* Hp = HeapA;
StgWord* HLimit = HeapA + HEAP_SIZE - 1;

/* pauses execution until a key is pressed */
void breakpoint() {
    printf("-- Breakpoint --\n");
    printf("Press any key to continue...\n");
    getchar();
}

/* dumps the contents of the active heap */
void dump_heap() {
    int i = 0;

    printf("\n-- Dumping heap --\n");
    printf("Hp = 0x%p [Base=0x%p,A=0x%p,B=0x%p]\n", Hp, Heap, HeapA, HeapB);

    for(; Heap + i < Hp; i++)
    {
        printf("Heap[%d] @ 0x%p = 0x%p\n", i, Heap + i, Heap[i]);
    }

    printf("-- Finished dumping heap --\n\n");
}

/* dumps the contents of the value stack */
void dump_val_stack() {
    int i = 1;

    printf("\n-- Dumping value stack --\n");
    printf("SpVal = 0x%p [Bottom=0x%p]\n", SpVal, Stack + STACK_SIZE - 1);

    for(; SpVal + i < Stack + STACK_SIZE; i++)
    {
        printf("SpVal[%d] = 0x%p\n", i, SpVal[i]);
    }

    printf("-- Finished dumping value stack --\n\n");
}

/* dumps the contents of the pointer stack */
void dump_ptr_stack() {
    StgWord* ptr = SpPtr - 1;

    printf("\n-- Dumping pointer stack --\n");
    printf("SpPtr = 0x%p [Top=0x%p]\n", SpPtr, Stack);

    for(; ptr >= Stack; ptr--)
    {
        printf("SpPtr[-%d] = 0x%p\n", SpPtr - ptr, *ptr);
    }

    printf("-- Finished dumping pointer stack --\n\n");
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

/* wrapper continuation which is the initial entry on the return stack */
void finished() {
    printf("\n");

    print_info();

    printf("-- Finished! --\n");
    printf("Ret: %d\n", Ret);
    exit(Ret);
}

/* the main entry point */
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
    while(TRUE) {
#ifdef DEBUG
        printf("Jumping to 0x%p...\n", cont);
#endif
        cont = (*cont)();
    }

    // we will probably never reach this point
    return 0;
}

/* stack overflow handler */
void stack_overflow()
{
    printf("\n!!! Stack overflow !!!\n\n");
    print_stack_info();
    exit(-1);
}

/* heap overflow handler */
CodeLabel run_gc()
{
    // store the address of the current node and acquire the address of
    // the evacuation and scavange code for it
    StgWord** CurrentNode = Node;
    CodeLabel NodeEvac = (CodeLabel)CurrentNode[0][1];
    CodeLabel NodeScav = (CodeLabel)CurrentNode[0][2];

    // initialise a pointer to the bottom of the pointer stack
    // and keep track of how much memory we have before/after GC
    StgWord* RootPtr = Stack;
    int usedFromMemory = Hp - Heap;
    int availFromMemory = HLimit - Hp;
    int usedToMemory = 0;
    int availToMemory = HEAP_SIZE;

    // report how much memory is currently used
    printf("\nRunning GC (%d words used, %d words free)...\n\n",
        usedFromMemory, availFromMemory);

    // 1. switch to the opposite heap and adjust the heap pointers to positions
    //    in the new heap








    // 2. run GC on the root set
#ifdef DEBUG
    printf("The root set consists of %d elements.\n", SpPtr - Stack);
#endif







    // 3. once garbage collection has completed on the root set, the evacuation
    // code for the closure pointed to by CurrentNode should return the address
    // of the corresponding closure in to-space -- we will want to continue
    // execution with that closure








    // calculate how much memory we have now
    usedToMemory = Hp - Heap;
    availToMemory = HLimit - Hp;

    printf("Garbage collection freed %d words (%d used, %d free)\n",
        availToMemory - availFromMemory, usedToMemory, availToMemory);

#ifdef DEBUG
    breakpoint();
#endif

    ENTER(Node);
}

CodeLabel forwarding_ptr_evac() {
    return (CodeLabel)Node[1];
}

CodeLabel forwarding_ptr_scavage() {
    return NULL;
}

StgWord _forwarding_ptr_info[] =
    {NULL,forwarding_ptr_evac,forwarding_ptr_scavage};
