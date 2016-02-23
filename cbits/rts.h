/*******************************************************************************
** Compiler for the STG Language                                              **
** By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             **
*******************************************************************************/

#ifndef rts_h
#define rts_h

#include <stdio.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0

#define HEAP_SIZE 1024
#define STACK_SIZE 1024

#define JUMP(lbl) return(lbl);

#ifdef DEBUG
#define ENTER(c) printf("ENTER(0x%p) => 0x%p\n", c, **c); JUMP(**c)
#else
#define ENTER(c) JUMP(**c)
#endif

typedef void* StgWord;

typedef void* (*CodeLabel)();
typedef CodeLabel** Closure;

extern CodeLabel main_entry();

/* Registers */
StgWord* SpVal;
StgWord* SpPtr;

StgWord** Node;
StgWord* RetVec;
int Ret;

/* Heap - grows from the top to the bottom */
StgWord HeapA[HEAP_SIZE];
StgWord HeapB[HEAP_SIZE];
StgWord* Heap;
StgWord* Hp;
StgWord* HLimit;

/* debugging helpers */
void dump_val_stack();
void dump_ptr_stack();
void dump_heap();

StgWord _forwarding_ptr_info[3];

#endif
