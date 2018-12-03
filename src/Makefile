.PHONY : all
all : grape.native grape.o
	gcc -c grape.c 

grape.native : 
	ocamlbuild -use-ocamlfind -r \
		-pkgs llvm,llvm.analysis \
		-cflags -w,+a-4 \
		grape.native

.PHONY : clean
clean :
	rm -rf _build grape.native *.o
