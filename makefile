.PHONY: all install clean

FILES = ops matrix poly galois rs
MLI = $(foreach file,$(FILES),$(file).mli)
CMI = $(foreach file,$(FILES),_build/$(file).cmi)

all:
	ocamlbuild -use-ocamlfind reedsolomon.cma
	ocamlbuild -use-ocamlfind reedsolomon.cmxa

install: all uninstall
	ocamlfind install reedsolomon META $(MLI) $(CMI) \
	_build/ops.cmti _build/poly.cmti _build/matrix.cmti _build/galois.cmti _build/rs.cmti \
		_build/reedsolomon.cma _build/reedsolomon.cmxa _build/reedsolomon.a

uninstall:
	- ocamlfind remove reedsolomon

clean:
	ocamlbuild -clean
	- find . -name "*~" | xargs rm -f
