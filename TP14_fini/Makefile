.PHONY: all clean

all: print_mnist mnist_to_pgm prioQueue main

mnist_to_pgm: mnist.cmx mnist_to_pgm.cmx
	ocamlopt -o $@ $^
print_mnist: mnist.cmx print_mnist.cmx
	ocamlopt -o $@ $^
main: mnist.cmx prioQueue.cmx knn.cmx main.cmx 
	ocamlopt -o $@ $^

clean:
	rm -f *.cmi *.cmo *.cmx *.o print_mnist mnist_to_pgm

%.cmx: %.ml
	ocamlopt -c -o $@ $<

%.cmi: %.mli
	ocamlopt -c -o $@ $<

# Dépendances générées automatiquement avec ocamldep
knn.cmo : \
    knn.cmi
knn.cmx : \
    knn.cmi
main.cmo : \
    mnist.cmi
main.cmx : \
    mnist.cmx
mnist.cmo : \
    mnist.cmi
mnist.cmx : \
    mnist.cmi
mnist_to_pgm.cmo : \
    mnist.cmi
mnist_to_pgm.cmx : \
    mnist.cmx
print_mnist.cmo : \
    mnist.cmi
print_mnist.cmx : \
    mnist.cmx
prioQueue.cmo : \
    prioQueue.cmi
prioQueue.cmx : \
    prioQueue.cmi
