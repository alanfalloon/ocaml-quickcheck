OCAMLC=ocamlc

test_List: quickCheck.cmo test_List.cmo
	$(OCAMLC) -g -o $@ $^

test_List.cmo: quickCheck.cmi

%.cmi: %.mli
	$(OCAMLC) -c -g -dtypes -o $@ $< $(OUTPUT_PROCESSING)

%.cmo %.cmi: %.ml
	$(OCAMLC) -c -g -dtypes -o $(@:%.cmi=%.cmo) $< $(OUTPUT_PROCESSING)

OUTPUT_PROCESSING:=$(if $(FLYMAKE), | sed -e '/:$$/{N;s%\n% %;}',)
