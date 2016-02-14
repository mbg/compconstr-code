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
#define ENTER(c) JUMP(**c)

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

/* Heap - grows from the bottom the top */
StgWord Heap[HEAP_SIZE];
StgWord* Hp;
StgWord* HLimit;

/* debugging helpers */
void dump_val_stack();

#endif
