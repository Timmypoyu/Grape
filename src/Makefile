.PHONY : all
all : clean grape.native grape.o

.PHONY : grape
grape.native :
	ocamlbuild -r -use-ocamlfind \
		-pkgs llvm,llvm.analysis \
		-cflags -w,+a-4 \
		grape.native

.PHONY : clean
clean :
	rm -f *.o

ast.cmo :
ast.cmx :
codegen.cmo : sast.cmo ast.cmo
codegen.cmx : sast.cmx ast.cmx
grape.cmo : semant.cmo sast.cmo codegen.cmo ast.cmo
grape.cmx : semant.cmx sast.cmx codegen.cmx ast.cmx
sast.cmo : ast.cmo
sast.cmx : ast.cmx
semant.cmo : sast.cmo ast.cmo
semant.cmx : sast.cmx ast.cmx
ast.cmo :
ast.cmx :
codegen.cmo : sast.cmo ast.cmo
codegen.cmx : sast.cmx ast.cmx
grape.cmo : semant.cmo sast.cmo codegen.cmo ast.cmo
grape.cmx : semant.cmx sast.cmx codegen.cmx ast.cmx
sast.cmo : ast.cmo
sast.cmx : ast.cmx
semant.cmo : sast.cmo ast.cmo
semant.cmx : sast.cmx ast.cmx
ast.cmo :
ast.cmx :
codegen.cmo : sast.cmo ast.cmo
codegen.cmx : sast.cmx ast.cmx
grape.cmo : semant.cmo sast.cmo codegen.cmo ast.cmo
grape.cmx : semant.cmx sast.cmx codegen.cmx ast.cmx
sast.cmo : ast.cmo
sast.cmx : ast.cmx
semant.cmo : sast.cmo ast.cmo
semant.cmx : sast.cmx ast.cmx
ast.cmo :
ast.cmx :
codegen.cmo : sast.cmo ast.cmo
codegen.cmx : sast.cmx ast.cmx
grape.cmo : semant.cmo sast.cmo codegen.cmo ast.cmo
grape.cmx : semant.cmx sast.cmx codegen.cmx ast.cmx
sast.cmo : ast.cmo
sast.cmx : ast.cmx
semant.cmo : sast.cmo ast.cmo
semant.cmx : sast.cmx ast.cmx
ast.cmo :
ast.cmx :
codegen.cmo : sast.cmo ast.cmo
codegen.cmx : sast.cmx ast.cmx
grape.cmo : semant.cmo sast.cmo codegen.cmo ast.cmo
grape.cmx : semant.cmx sast.cmx codegen.cmx ast.cmx
sast.cmo : ast.cmo
sast.cmx : ast.cmx
semant.cmo : sast.cmo ast.cmo
semant.cmx : sast.cmx ast.cmx
ast.cmo :
ast.cmx :
codegen.cmo : sast.cmo ast.cmo
codegen.cmx : sast.cmx ast.cmx
grape.cmo : semant.cmo sast.cmo codegen.cmo ast.cmo
grape.cmx : semant.cmx sast.cmx codegen.cmx ast.cmx
sast.cmo : ast.cmo
sast.cmx : ast.cmx
semant.cmo : sast.cmo ast.cmo
semant.cmx : sast.cmx ast.cmx
