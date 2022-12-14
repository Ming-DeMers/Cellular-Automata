.PHONY: test check
.PHONY: gui check

build:
	dune clean
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
	rm -f adventure.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

gui:  
	OCAMLRUNPARAM=b dune exec gui/gui.exe

