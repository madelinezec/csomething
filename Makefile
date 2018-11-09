RESULT = csomething
SOURCES = src/lexer.ml \ src/parser.ml\ 

all: native-code

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
