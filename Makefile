all: z

z:
	ocamlbuild -use-ocamlfind z.native

clean:
	ocamlbuild -clean
