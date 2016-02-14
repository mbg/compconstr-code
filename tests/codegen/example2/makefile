all: builtin

builtin: BuiltIn.stg
	../../../dist/build/stg/stg BuiltIn.stg
	gcc BuiltIn.c ../../../cbits/rts.c -I../../../cbits/
