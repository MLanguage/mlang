clean: 
	rm -f *.cmo *.cmi test_lexer.ml test_parser.ml test_parser.mli

# Compiling test file parser
types_module.cmo: 
	ocamlc.opt -c $(DEBUG_FLAG) types_module.ml

test_parser.ml: types_module.cmo
	menhir --infer-write-query mock.ml test_parser.mly
	ocamlc.opt -i mock.ml > reply
	menhir --infer-read-reply reply test_parser.mly
	rm mock.ml reply

test_parser.cmo: test_parser.ml
	ocamlc.opt -c $(DEBUG_FLAG) test_parser.mli test_parser.ml

test_lexer.ml: test_parser.cmo
	ocamllex test_lexer.mll
	
test_lexer.cmo: test_lexer.ml
	ocamlc.opt -c $(DEBUG_FLAG) test_lexer.ml
	
fip.cmo: test_lexer.cmo test_parser.cmo
	ocamlc.opt -c $(DEBUG_FLAG) fip.ml