all: main

main:
	ocamlbuild -use-ocamlfind main.native

clean:
	ocamlbuild -clean
