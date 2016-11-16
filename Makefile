OBJS = core.cmo fol_lexer.cmo fol_parser.cmo printer.cmo
OCAMLC = ocamlfind ocamlc -package batteries -linkpkg

.PHONY: default clean run

default: main

main: $(OBJS)
	ocamlfind ocamlmktop -package batteries -linkpkg -I `camlp5 -where` camlp5o.cma $(OBJS) -o $@

fol_parser.cmo: fol_lexer.cmo
printer.cmo: core.cmo

%.cmo: %.ml
	$(OCAMLC) -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo pa_extprint.cmo pa_lexer.cmo q_ast.cmo' $<

clean:
	rm -rf *.cmi *.cmo main

run: main
	@socat READLINE EXEC:'./main -init init'
