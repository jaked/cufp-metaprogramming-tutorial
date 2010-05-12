BFILES=_build/JsonComprehension.cmo

all:
	ocamlbuild JsonComprehension.cmo

install:
	ocamlfind install json_compr META $(BFILES)

uninstall:
	ocamlfind remove json_compr

clean:
	ocamlbuild -clean
