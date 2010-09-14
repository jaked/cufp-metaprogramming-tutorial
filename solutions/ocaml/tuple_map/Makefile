_build/tuple_map.cmo:
	ocamlbuild tuple_map.cmo

test.byte:
	ocamlbuild test.byte

install:
	ocamlfind install tuple_map META _build/tuple_map.cmo

uninstall:
	ocamlfind remove tuple_map

clean:
	ocamlbuild -clean

.PHONY: _build/tuple_map.cmo
