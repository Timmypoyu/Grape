.PHONY : all
all : clean grape.native grape.o

.PHONY : grape
grape.native :
	ocamlbuild -use-ocamlfind -r \
		-pkgs llvm,llvm.analysis \
		-cflags -w,+a-4 \
		grape.native

.PHONY : clean
clean :
	rm -f *.o grape.native
