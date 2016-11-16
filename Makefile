OBJS = core.cmo
OCAMLC = ocamlfind ocamlc -package batteries -linkpkg

.PHONY: default clean

default: $(OBJS)

%.cmi: %.mli
	$(OCAMLC) -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo pa_extprint.cmo pa_lexer.cmo q_MLast.cmo' $<

%.cmo: %.ml
	$(OCAMLC) -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo pa_extprint.cmo pa_lexer.cmo q_MLast.cmo' $<

clean:
	rm -rf *.cmi *.cmo
