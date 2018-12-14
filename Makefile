RESULT = csomething
SOURCES = src/lexer.ml src/parser.ml src/ast.ml src/driver.ml 
PACKS = ppx_deriving.std
PP = camlp4find $(PACKS)
export PP

all: native-code

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
