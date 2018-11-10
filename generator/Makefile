# "make clean" removes all generated files

.PHONY: all
all : csomething

.PHONY : clean
clean :
	rm -rf csomething scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.s *.ll *.out *.exe

OBJS = ast.cmo  parser.cmo scanner.cmo csomething.cmo

csomething : $(OBJS)
	ocamlc -o csomething $(OBJS)

csomething.cmo:
	ocamlc -c csomething.ml

ast.cmo: ast.ml
	ocamlc -c ast.ml

parser.mli: 
	ocamlyacc parser.mly

scanner.ml: 
	ocamllex scanner.mll

parser.ml: parser.mli
	ocamlc -c  parser.mli

parser.cmo: parser.ml
	ocamlc -c  parser.ml

scanner.cmo: scanner.ml
	ocamlc -c  scanner.ml


