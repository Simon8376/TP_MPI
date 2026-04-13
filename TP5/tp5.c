#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>


void* thread_1(void* arg){
    int* i = (int*)arg;
    for (int j = 0; j < 10000; j++){
        *i = *i +1;
        printf("%d\n", *i);
    }
    pthread_exit(EXIT_SUCCESS);
}

pthread_mutex_t mutex;

void* thread_2(void* arg){
    int* i = (int*)arg;
    for (int j = 0; j < 10000; j++){
        pthread_mutex_lock(&mutex);
        *i = *i +1;
        pthread_mutex_unlock(&mutex);
    }
    pthread_exit(EXIT_SUCCESS);
}


int main(void){
    // Sans les mutex
    int i = 0;
    pthread_t thread1;
    pthread_t thread2;
    pthread_create(&thread1, NULL, thread_1, &i);
    pthread_create(&thread2, NULL, thread_1, &i);
    pthread_join(thread1, NULL);
    pthread_join(thread2, NULL);
    printf("%d\n", i);

    //Avec mutex:
    i = 0;
    printf("AVEC LES MUTEX\n");
    pthread_t thread3;
    pthread_t thread4;
    pthread_create(&thread3, NULL, thread_2, &i);
    pthread_create(&thread4, NULL, thread_2, &i);
    pthread_join(thread3, NULL);
    pthread_join(thread4, NULL);
    printf("%d", i);
    return 0;
}

// Erreur de parallélisme sans les printf, plus d'erreur avec les printf vu que le printf est HYPER LONG EN VRAI