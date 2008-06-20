OCAMLC=ocamlc
quickCheck.cmo: quickCheck.ml
	$(OCAMLC) -c -g -dtypes -o $@ $<
