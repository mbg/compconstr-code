all: algtoprim

algtoprim: AlgToPrim.stg
	../../../dist/build/stg/stg AlgToPrim.stg
	gcc AlgToPrim.c ../../../cbits/rts.c -I../../../cbits/
