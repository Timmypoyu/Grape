.PHONY : all
all : clean grape.native

.PHONY : grape
grape.native :
	ocamlbuild -use-ocamlfind -r \
		-pkgs llvm,llvm.analysis \
		-cflags -w,+a-4 \
		grape.native

.PHONY : clean
clean :
	rm -rf _build grape.native
