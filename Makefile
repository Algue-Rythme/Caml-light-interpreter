all:
	ocamlbuild -yaccflag -v -lib unix main.native; ln -fs main.native f2bdd
	ocamlbuild main_tests.native; ln -fs main_tests.native generate

byte:
	ocamlbuild -yaccflag -v main.byte

clean:
	ocamlbuild -clean
