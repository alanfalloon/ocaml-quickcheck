OCAMLC=ocamlc
quickCheck.cmo: quickCheck.ml
	$(OCAMLC) -c -g -o $@ $<
