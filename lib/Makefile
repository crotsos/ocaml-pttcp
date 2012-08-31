all: 
	./cmd config
	echo open Pttcp > main.ml
	echo "let _ = OS.Main.run (main ())" >> main.ml
	ocamlbuild -classic-display main.native

xen: 
	./cmd config
	echo open Pttcp > main.ml
	echo "let _ = OS.Main.run (main ())" >> main.ml
	ocamlbuild -classic-display main.xen

.PHONY:clean
clean:
	rm -f main.ml
	ocamlbuild -clean
