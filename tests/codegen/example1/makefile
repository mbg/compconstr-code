all: prim

prim: Prim.stg
	../../../dist/build/stg/stg Prim.stg
	gcc Prim.c ../../../cbits/rts.c -I../../../cbits/
