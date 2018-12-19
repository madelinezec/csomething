RESULT = csomething
SOURCES = src/scanner.mll src/parser.mly src/ast.ml src/symbol.ml src/semantics.ml src/semcheck.ml src/codegen.ml src/csomething.ml
PACKS = ppx_deriving.std llvm
PP = camlp4find $(PACKS)
export PP

all: native-code

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
