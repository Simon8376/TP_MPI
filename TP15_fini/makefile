.PHONY: all clean

all: main run

main: main.cmo
	ocamlfind ocamlc -linkpkg -package unix,graphics $< -o $@

main.cmo: main.ml
	ocamlfind ocamlc -c -package unix,graphics $< -o $@

clean:
	rm -f main.cmo main.cmi main

run: main.cmo 
	./main
