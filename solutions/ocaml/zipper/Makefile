_build/zipper.cmo:
	ocamlbuild zipper.cmo

test.byte:
	ocamlbuild test.byte

install:
	ocamlfind install zipper META _build/zipper.cmo

uninstall:
	ocamlfind remove zipper

clean:
	ocamlbuild -clean

.PHONY: _build/zipper.cmo
