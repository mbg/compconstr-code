all: letrec c

letrec: LetRec.stg
	../../../dist/build/stg/stg LetRec.stg

c:
	gcc LetRec.c ../../../cbits/rts.c -I../../../cbits/
