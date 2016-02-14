all: map

map: Map.stg
	../dist/build/stg/stg Map.stg
	gcc Map.c ../cbits/rts.c -I../cbits/

c: Map.c
	gcc Map.c ../cbits/rts.c -I../cbits/
