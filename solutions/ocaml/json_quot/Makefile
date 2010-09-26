FILES=\
json_quot.cma \
jq_ast.mli jq_ast.cmi \
jq_parser.mli jq_parser.cmi \
jq_printer.mli jq_printer.cmi \
jq_top.cmo

BFILES=$(addprefix _build/,$(FILES))

all:
	ocamlbuild json_quot.cma jq_top.cmo

install:
	ocamlfind install json_quot META $(BFILES)

uninstall:
	ocamlfind remove json_quot

clean:
	ocamlbuild -clean
