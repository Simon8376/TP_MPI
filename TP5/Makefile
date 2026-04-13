PHONY: all clean

all: boite_noire main partie5

boite_noire2.o: boite_noire2.c boite_noire2.h
	gcc -Wall -pedantic -fsanitize=address -c boite_noire2.c

test.o: test.c boite_noire2.h
	gcc -Wall -pedantic -fsanitize=address -c test.c

boite_noire: test.o boite_noire2.o
	gcc -pthread -Wall -pedantic -fsanitize=address -o boite_noire boite_noire2.o test.o

main.o: main.c boite_noire2.h
	gcc -Wall -pedantic -fsanitize=address -c main.c

main: main.o
	gcc -pthread -Wall -pedantic -fsanitize=address -o main boite_noire2.o main.o

run_main: main
	./main

run_boite_noire: boite_noire 
	./boite_noire

partie5: tp5_partie5.c 
	gcc -Wall -pedantic -fsanitize=address -o part5 tp5_partie5.c
	./part5

archivage:
	tar -zcvf tab.tar *c *h Makefile

clean: 
	rm -f *.o main

 
