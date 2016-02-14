all: boxed

boxed: Boxed.stg
	../../../dist/build/stg/stg Boxed.stg
	gcc Boxed.c ../../../cbits/rts.c -I../../../cbits/

c:
	gcc Boxed.c ../../../cbits/rts.c -I../../../cbits/
