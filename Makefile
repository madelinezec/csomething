RESULT = csomething
SOURCES = src/lexer.ml\

all: native-code

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
