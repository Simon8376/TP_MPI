#include <semaphore.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>


typedef struct barriere{
    pthread_mutex_t m;
    int p; //Initialisé à n
    sem_t s2; //Initialisé à 0
    int p2; //Initialisé à 0
}barriere;


void attendre_tous(barriere* b, int n){
    pthread_mutex_lock(&(b -> m));
    b -> p = b -> p - 1;
    if (b -> p == 0){
        sem_post(&(b -> s2));
    }
    pthread_mutex_unlock(&(b -> m));

    sem_wait(&(b -> s2));

    pthread_mutex_lock(&(b -> m));
    b -> p2 = b -> p2 +1;
    if (b -> p2 == n){ // On ferme la barriere et on remet les variables a l'état initial
        b -> p = n;
        b -> p2 = 0;
    }
    else {
        sem_post(&(b -> s2));
    }
    pthread_mutex_unlock(&(b -> m));
}

// Partie 6
// Oui: imaginons un tableau [3; 2; 1]. Le premier lutin switch en même temps que le deuxième
// donc on se retrouve avec [2; 1 (ou 3); 2] ce qui ne correspond pas


void swap(int* liste, int i, int j){
    int k = liste[i];
    liste[i] = liste[j];
    liste[j] = k;
}

typedef struct argu{
    int i;
    int j;
    int* liste;
    pthread_mutex_t* mut;
    pthread_t* thread;
}argu;

void* t(void* arg){
    argu* val = (argu*)arg;

    pthread_mutex_lock(&(val -> mut[val -> i]));
    pthread_mutex_unlock(&(val -> mut[val -> j]));
    if (val -> liste[val -> i] > val -> liste[val -> j]){
        swap(val -> liste, val -> i, val -> j);
    }
    pthread_mutex_unlock(&(val -> mut[val -> i]));
    pthread_mutex_unlock(&(val -> mut[val -> j]));

    pthread_exit(EXIT_SUCCESS);
}


void tri_lutin(int* liste, int n){
    pthread_mutex_t* mut = (pthread_mutex_t*)malloc(sizeof(pthread_mutex_t) * n);
    pthread_t* thread = (pthread_t*)malloc(sizeof(pthread_t) * (n-1));

    for (int i = 0; i<n; i++){
        for (int j = 1; j<n; j++){
            argu* a = (argu*)malloc(sizeof(argu));
            a -> i = j-1;
            a -> j = j;
            a -> liste = liste;
            a -> mut = mut;
            a -> thread = thread;
            pthread_create(&thread[i], NULL, t, a);
        }
    }
    for (int i = 0; i<n; i++){
        pthread_join(thread[i], NULL);
    }

    free(mut);
    free(thread);
}

void affiche(int* l, int n){
    for (int i = 0; i<n; i++){
        printf("%d, ", l[i]);
    }
}

void partition(int* liste, int n, int pivot){
    int min_plus_grand = 0;
    for (int i = 1; i < n; i++){
        if (liste[i] < liste[pivot]){
            swap(liste, i, pivot);
            min_plus_grand ++;
        }
    }
}


int main(){
    int* l = (int*)malloc(sizeof(int)*5);
    for (int i = 0; i<5; i++){
        l[i] = 5-i;
    }
    tri_lutin(l, 5);
    affiche(l, 5);

    free(l);
}



