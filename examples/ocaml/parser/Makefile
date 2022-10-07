include ../Makefile-generic-ocaml-rules.include

clean: 
	rm -f *.cmo *.cmi *.cmx *.o test_lexer.ml test_parser.ml test_parser.mli

# Compiling test file parser

test_parser.mli test_parser.ml: types_module.cmo types_module.cmx
	menhir --infer-write-query mock.ml test_parser.mly
	ocamlc.opt -i mock.ml > reply
	menhir --infer-read-reply reply test_parser.mly
	rm mock.ml reply

test_lexer.ml: test_parser.cmi test_parser.cmo test_parser.cmx
	ocamllex test_lexer.mll

fip.cmo: test_lexer.cmo

fip.cmx: test_lexer.cmx