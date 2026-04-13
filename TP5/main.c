#include "boite_noire2.h"
#include <stdlib.h>
#include <pthread.h>
#include <stdio.h>

pthread_mutex_t m;


typedef struct data{
    int* j;
    int* next_up;
    boite_noire* boite;
    int* libre;
}data;

typedef struct binome{
    data* d;
    int j;
}binome;

void* t(void* arg){
    binome* b = (binome*)arg;
    b -> d -> libre[b -> j] = 0;
    pthread_mutex_lock(&m);
    *(b -> d -> next_up) = *(b -> d -> next_up) +1;
    pthread_mutex_unlock(&m);
    boite_noire_tache(b -> d -> boite, *(b -> d -> next_up) -1);
    printf("Tache %d ok\n", *(b -> d -> next_up) -1);
    b -> d -> libre[b -> j] = 1;
    pthread_exit(EXIT_SUCCESS);
}



int main(){
    int* libre = (int*)malloc(sizeof(int)*9);
    for (int j = 0; j < 9; j++){
        libre[j] = 1;
    }

    pthread_t* thread = (pthread_t*)malloc(sizeof(pthread_t) * 9);
    boite_noire* b1 = boite_cree_1();

    data* d1 = (data*)malloc(sizeof(data));
    int next_up = 0;

    d1 -> next_up = &next_up;
    d1 -> boite = b1;
    d1 -> libre = libre;

    binome** bin = (binome**)malloc(sizeof(binome*)*9);

    for (int j = 0; j < 9; j++){
        bin[j] = (binome*)malloc(sizeof(binome));
        bin[j] -> j = j;
        bin[j] -> d = d1;
    }

    boite_demarre(d1 -> boite, false);

    while (*(d1 -> next_up) < boite_objectif(b1)){
        for (int j = 0; j < 9; j++){
            if (libre[j] == 1){
                pthread_create(&thread[j], NULL, t, bin[j]);
            }
        }
    }

    printf("FINITO");

    for (int i = 0; i < 9; i++){
        pthread_join(thread[i], NULL);
    }

    free(d1);
    free(thread);
    for (int j = 0; j < 9; j++){
        free(bin[j]);
    }
    free(libre);
    free(bin);
    return 0;
}
