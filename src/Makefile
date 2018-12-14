.PHONY : all
all : grape.native stdlib.o
	cp grape ../dist
	mv stdlib.o ../dist
	cp _build/grape.native ../dist

stdlib.o: 
	cd stdlib && gcc -g -O -c ./*.c
	ld -r stdlib/*.o -o stdlib.o

grape.native : 
	ocamlbuild -use-ocamlfind -tag debug -r \
		-pkgs llvm,llvm.analysis \
		-cflags -w,+a-4 \
		grape.native

.PHONY : clean
clean :
	rm -rf _build grape.native **/*.o parser.ml parser.mli
