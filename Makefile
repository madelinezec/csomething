RESULT = csomething
SOURCES = src/scanner.mll src/parser.mly src/ast.ml src/csomething.ml src/symbol.ml src/semantics.ml
PACKS = ppx_deriving.std
PP = camlp4find $(PACKS)
export PP

all: native-code

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
