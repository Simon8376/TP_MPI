#include "automata.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

// Si aucune transition n'est étiquetée par a, alors a n'est pas dans le langage


bool automaton_accepts(const automaton* a, unsigned const char* mot){
    int state = 0;
    for (int j = 0; mot[j] != '\0'; j++){
        bool b = false;
        for (int i = a -> first_transition[(int)mot[j]]; i < a -> first_transition[(int)mot[j+1]]-1; i++){
            if (!(b) && a -> transitions[i].src_state == state){
                state = a -> transitions[i].dest_state;
                b = true;
            }
        }
        if (!(b)){
            return false;
        }
    }
    return a -> is_final[state];
}


void automatest(void){
    //2
    automaton a;
    bool is_final[5] = {false, false, false, true};
    a.is_final = is_final;
    transition t[15];
    // Le t correspond au 0; le o au 1; tout le reste au 2
    t[0].src_state = 0;
    t[0].dest_state = 1;
    t[1].src_state = 2;
    t[1].dest_state = 3;
    t[2].src_state = 1;
    t[2].dest_state = 0;
    t[3].src_state = 3;
    t[3].dest_state = 0;
    t[4].src_state = 4;
    t[4].dest_state = 0;

    t[5].src_state = 1;
    t[5].dest_state = 2;
    t[6].src_state = 3;
    t[6].dest_state = 4;
    t[7].src_state = 0;
    t[7].dest_state = 0;
    t[8].src_state = 2;
    t[8].dest_state = 0;
    t[9].src_state = 4;
    t[9].dest_state = 0;

    t[10].src_state = 0;
    t[10].dest_state = 0;
    t[11].src_state = 1;
    t[11].dest_state = 0;
    t[12].src_state = 2;
    t[12].dest_state = 0;
    t[13].src_state = 3;
    t[13].dest_state = 0;
    t[14].src_state = 4;
    t[14].dest_state = 4;

    a.first_transition[0] = 0;
    a.first_transition[1] = 5;
    a.first_transition[2] = 10;
    a.first_transition[3] = 15;

    a.nb_states = 5;
    a.transitions = t;

    //assert(automaton_accepts(&a, "0101"));
    //assert(automaton_accepts(&a, "22201012222"));
    //assert(!(automaton_accepts(&a, "21022210")));
    //assert(automaton_accepts(&a, "toto"));

}




automaton* automaton_read(FILE* fh){ // On considère qu'on peut borner le nombre d'états mashallah, sinon il faudrait faire deux parcours ou des tableaux dynamiques
    automaton* a = (automaton*)malloc(sizeof(automaton));
    a -> is_final = (bool*)malloc(sizeof(bool) * 100);

    transition** trans = (transition**)malloc(sizeof(transition*) * 256);
    for (int i = 0; i < 256; i++){
        trans[i] = (transition*)malloc(sizeof(transition) * 100);
    }

    int* taille = (int*)malloc(sizeof(int) * 256);
    for (int i = 0; i < 256; i++){
        taille[i] = 0;
    }
    char c;
    int n;
    while (c != ';'){
        printf("%c\n", c);
        int m = fscanf(fh, "%d%c", &n, &c);
        printf("%d\n", m);
        a -> is_final[n] = true;
    }
    int dep;
    int arr;
    char l;
    char ligne[80];
    char mot[80];
    while (fscanf(fh, "%[;\n]", ligne) == 1){
        printf("%s\n", ligne);
        if (sscanf(ligne, "%d ->[%c] %d;\n", &dep, &l, &arr) == 3){
            trans[(int)l][taille[(int)l]] = (transition){dep, arr};
            taille[(int)l]++;
        }
        else if (sscanf(ligne, "%d -> %d;\n", &dep, &arr) == 2){
            for (int i = 0; i < 256; i++){
                trans[i][taille[i]] = (transition){dep, arr};
                taille[i]++;
            }
        }
        else if (sscanf(ligne, "%d ->![%s] %d;\n", &dep, mot, &arr) == 3){
            for (int i = 0; i < 256; i++){
                bool b = true;
                for (int j = 0; j < strlen(mot); j++){
                    if (i == (int)mot[j]) b = false;
                }
                if (b){
                    trans[i][taille[i]] = (transition){dep, arr};
                    taille[i]++;
                }
            }
        }
        else printf("Crac");
    }

    int len = 0; // Nombre de transitions
    for (int i = 0; i < 256; i++){
        len = len + taille[i];
    }
    
    
    transition* transitions = (transition*)malloc(sizeof(transition) * len);
    int t = 0;
    for (int i = 0; i < 256; i++){
        a -> first_transition[i] = t;
        for (int j = 0; j < taille[i]; j++){
            transitions[t] = trans[i][j];
            t = t +1;
        }
    }
    a -> first_transition[257] = t;


    a -> transitions = transitions;

    free(trans);
    free(taille);

    return a;
}


void afficher_sommets(automaton* a){
    for (int i = 0; i < a -> nb_states -1; i++){
        if (a -> is_final[i]) printf("%d is final\n", i);
        else printf("%d is not final\n", i);
    }
    printf("\n");
}


void afficher_transitions(automaton* a){
    int nb = 0;
    for (int i = 0; i < a -> first_transition[257]; i++){
        printf("Transition de %d à %d de symbole %c\n", a -> transitions[i].src_state, a -> transitions[i].dest_state, nb);
        if (i >= a -> first_transition[nb +1]) nb++;
    }
}



int main(int argc, char* argv[]){
    FILE* fh = fopen(argv[1], "r");
    automaton* automat = automaton_read(fh);
    //afficher_sommets(automat);
    //afficher_transitions(automat);
    return 0;
}
