all: gc c

gc: gc.stg
	../../dist/build/stg/stg gc.stg

c: gc.c
	gcc gc.c ../../cbits/rts.c -I../../cbits/
