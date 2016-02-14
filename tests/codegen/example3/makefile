all: truth

truth: TruthTest.stg
	../../../dist/build/stg/stg TruthTest.stg
	gcc TruthTest.c ../../../cbits/rts.c -I../../../cbits/
