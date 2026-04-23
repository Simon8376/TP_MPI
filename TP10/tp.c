#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include <stdio.h>
#include <stdbool.h>

void init_alea(){
    srand(time(NULL));
}

// On est dans [|0, N-1|] et c'est pas uniforme vu que les entiers entre INT_MAX et la derniere tranche multiple de n est surreprésentée

int uniforme(int n){
    return rand() % n;
}

typedef struct graph{
    int n;
    int* degre;
    int** adj; // Tableau de taille n de tableau de taille degre[i] en mode liste d'adjacence
}graph;


void melange_liste(int* liste, int n){
    for (int i = 0; i < n; i++){
        int j = uniforme(i+1);
        int k = liste[i];
        liste[i] = liste[j];
        liste[j] = k;
    }
}


graph* graphe_genere(int n, int p){
    graph* g = (graph*)malloc(sizeof(graph));
    g -> adj = (int**)malloc(sizeof(int*) * n);
    g -> degre = (int*)malloc(sizeof(int));
    for (int i = 0; i < n; i++){
        g -> adj[i] = (int*)malloc(sizeof(int) * n);
        g -> degre[i] = 0;
    }
    for (int i = 0; i < n; i++){
        for (int j = 0; j < i; j++){
            int chance = uniforme(1000000);
            if (chance < p*1000000){
                g -> adj[i][g -> degre[i]] = j;
                g -> adj[j][g -> degre[j]] = i;
                g -> degre[i]++;
                g -> degre[j]++;
            }
        }
    }
    int** adj2 = (int**)malloc(sizeof(int*) * n);
    for (int i = 0; i < n; i++){
        adj2[i] = (int*)malloc(sizeof(int) * g -> degre[i]);
        for (int j = 0; j < g -> degre[i]; j++){
            adj2[i][j] = g -> adj[i][j];
        }
        free(g -> adj[i]);
        melange_liste(adj2[i], g -> degre[i]);
    }
    free(g -> adj);
    g -> adj = adj2;
    g -> n = n;
    return g;
}


// E(Gnp) = n*(n-1)/2 * p


void libere(graph* g){
    for (int i = 0; i < g -> n; i++){
        free(g -> adj[i]);
    }
    free(g -> adj);
    free(g -> degre);
    free(g);
}



// voir feuille tp


//Question 7:

typedef struct couple{
    int a;
    int b;
}couple;


couple reciphi(int n){
    couple c;
    c.a = 1;
    while (c.a * (c.a -1)/2 <= n){
        c.a = c.a +1;
    }
    c.a = c.a -1;
    c.b = n - c.a * (c.a-1)/2;
    return c;
}


graph* graphe_genere_aretes(int n, int m){
    int borne = n * (n-1)/2;
    int* liste = (int*)malloc(sizeof(int)* borne);
    for (int i = 0; i < borne; i++){
        liste[i] = i;
    }
    melange_liste(liste, borne);
    graph* g = (graph*)malloc(sizeof(graph));
    g -> adj = (int**)malloc(sizeof(int*) * n);
    g -> degre = (int*)malloc(sizeof(int) * n);
    for (int i = 0; i < n; i++){
        g -> adj[i] = (int*)malloc(sizeof(int) * n);
        g -> degre[i] = 0;
    }
    for (int i = 0; i < m; i++){
        couple c = reciphi(i);
        g -> adj[c.a][g -> degre[c.a]] = c.b;
        g -> adj[c.b][g -> degre[c.b]] = c.a;
        g -> degre[c.a] ++;
        g -> degre[c.b]++;
    }

    int** adj2 = (int**)malloc(sizeof(int*) * n);
    for (int i = 0; i < n; i++){
        adj2[i] = (int*)malloc(sizeof(int) * g -> degre[i]);
        for (int j = 0; j < g -> degre[i]; j++){
            adj2[i][j] = g -> adj[i][j];
        }
        free(g -> adj[i]);
        melange_liste(adj2[i], g -> degre[i]);
    }
    free(g -> adj);
    free(liste);
    g -> adj = adj2;
    g -> n = n;
    return g;
}


typedef struct tab{
    int* tab;
    int t;
    int ind;
}tab;


void rotation(tab* m, int i, int k){
    assert(i < k);
    int* res = (int*)malloc(sizeof(int) * (k+1));
    for (int ind = i+1; ind < k+1; ind++){
        res[ind] = m -> tab[-ind + i + 1 + k];
    }
    for (int i = 0; i < k+1; i++){
        m -> tab[i] = res[i];
    }
    free(res);
}


void ajout(tab* tab, int val){
    if (tab -> ind < tab -> t){
        tab -> tab[tab -> ind] = val;
        tab -> ind++;
    }
    else{
        printf("Impossible\n");
    }
}


tab* algo(graph* g){
    int** unused = malloc(sizeof(int*) * g -> n);
    int* nb_unused = malloc(sizeof(int) * g -> n);
    for (int i = 0; i < g -> n; i++){
        nb_unused[i] = g -> degre[i];
        unused[i] = malloc(sizeof(int) * g -> degre[i]);
        for (int j = 0; j < g -> degre[i]; j++){
            unused[i][j] = g -> adj[i][j];
        }
    }

    tab* ch = malloc(sizeof(tab));
    ch -> tab = malloc(sizeof(int) * g -> n);
    ch -> t = g -> n;
    ch -> ind = 1;
    ch -> tab[0] = uniforme(g -> n);

    while (ch -> ind != g -> n){
        int vk = ch -> tab[ch -> ind];
        if (nb_unused[vk] == 0){
            printf("Echec");
        }
        else{
            int u = unused[vk][0];
            unused[vk][0] = unused[vk][nb_unused[vk] -1];
            nb_unused[vk]--;
            
            for (int i = 0; i < nb_unused[u]; i++){
                if (unused[u][i] == vk){
                    unused[u][i] = unused[u][nb_unused[u] -1];
                    nb_unused[u]--;
                    i = nb_unused[u];
                }
            }

            bool b = true;
            int j;
            for (int i = 0; i < ch -> ind; i++){
                if (u == ch -> tab[i]){
                    b = false;
                    j = i;
                }
            }
            if (b) ajout(ch, u);
            else{
                rotation(ch, j, ch -> ind -1);
            }
        }
    }

    for (int i = 0; i < g -> n; i++){
        free(unused[i]);
    }
    free(unused);
    free(nb_unused);
    return ch;
}



int main(){
    graph* g = graphe_genere_aretes(10, 20);
    tab* ch = algo(g);
    return 0;
}