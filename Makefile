OCAMLC=ocamlc
quickCheck.cmo: quickCheck.ml
	$(OCAMLC) -c -g -dtypes -o $@ $< $(OUTPUT_PROCESSING)
OUTPUT_PROCESSING:=$(if $(FLYMAKE), | sed -e '/:$$/{N;s%\n% %;}',)
