example:
	ocamlbuild -I src -I utils examples/example.native
	./example.native

test:
	ocamlbuild -package ounit2 -I src tests/jparser_test.native
	./jparser_test.native

clean:
	ocamlbuild -clean
