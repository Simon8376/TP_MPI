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
    int** adj; // Tableau de taille n de tableau de taille n en mode liste d'adjacence
}graph;


void melange_liste(int* liste, int n){
    for (int i = 0; i < n; i++){
        int j = uniforme(i+1);
        int k = liste[i];
        liste[i] = liste[j];
        liste[j] = k;
    }
}


graph* graphe_genere(int n, float p){
    graph* g = malloc(sizeof(graph));
    g -> n = n;
    g -> adj = malloc(sizeof(int*) * n);
    g -> degre = malloc(sizeof(int) * n);
    for (int i = 0; i < n; i++){
        g->adj[i] = malloc(sizeof(int) * n);
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
    for (int i = 0; i < n; i++){
        melange_liste(g -> adj[i], g->degre[i]);
    }
    return g;
}


// E(Gnp) = n*(n-1)/2 * p


void libere(graph* g){
    for (int i = 0; i < g->n; i++){
        free(g->adj[i]);
    }
    free(g -> adj);
    free(g -> degre);
    free(g);
}



void test_genere(void){
    for (int n = 3; n < 10; n++){
        int sum = 0;
        for (int j = 0; j < 10; j++){
            graph* g = graphe_genere(n, 0.5);
            int c = 0;
            for (int i = 0; i < g->n; i++){
                c = c + g->degre[i];
                for (int j = 0; j < g->degre[i]; j++){
                    fprintf(stderr, "%d\n", g->adj[i][j]);
                }
            }
            sum = sum + c;
            libere(g);
        }
        printf("Have %d aretes, expected %d\n", sum/20, n*(n-1)/4);
    }
}


void test_genere_bis(void){
    graph* g = graphe_genere(10, 0.25);
    for (int i = 0; i < g->n; i++){
        fprintf(stderr, "%d: ", i);
        for (int j = 0; j < g->degre[i]; j++){
            fprintf(stderr, "%d, ", g->adj[i][j]);
        }
        fprintf(stderr, "\n");
    }
    libere(g);
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
    assert(m <= borne);
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
        couple c = reciphi(liste[i]);
        g -> adj[c.a][g -> degre[c.a]] = c.b;
        g -> adj[c.b][g -> degre[c.b]] = c.a;
        g -> degre[c.a] ++;
        g -> degre[c.b]++;
    }
    free(liste);

    for (int i = 0; i < n; i++){
        melange_liste(g->adj[i], g->degre[i]);
    }
    g -> n = n;
    return g;
}

void test_genere_aretes(void){
    for (int n = 3; n < 10; n++){
        for (int j = 0; j < n; j++){
            graph* g = graphe_genere_aretes(n, j);
            int c = 0;
            for (int i = 0; i < g->n; i++){
                c = c + g->degre[i];
                for (int j = 0; j < g->degre[i]; j++){
                    fprintf(stderr, "%d\n", g->adj[i][j]);
                }
            }
            libere(g);
            printf("Have %d aretes, expected %d\n", c/2, j);
        }
    }
}

void test_genere_aretes_bis(int n, int m){
    graph* g = graphe_genere_aretes(n, m);
    for (int i = 0; i < g->n; i++){
        fprintf(stderr, "%d: ", i);
        for (int j = 0; j < g->degre[i]; j++){
            fprintf(stderr, "%d, ", g->adj[i][j]);
        }
        fprintf(stderr, "\n");
    }
    libere(g);
}


typedef struct tab{
    int* tab;
    int t; //Longeur de tab
    int ind; //Position de l'ajout
    bool fail; //Indique si le chemin est une solution ou pas
}tab;


void rotation(tab* m, int i){
    assert(i+1 < m->ind);
    int k = m -> ind-1;
    assert(i < k);
    int* res = (int*)malloc(sizeof(int) * (k-i));
    for (int ind = 0; ind < k-i; ind++){
        res[ind] = m -> tab[k-ind];
    }
    for (int ind = 0; ind < k-i; ind++){
        m -> tab[ind+i+1] = res[ind];
    }
    free(res);
}


void affiche_chemin(tab* ch){
    for (int i = 0; i < ch -> ind; i++){
        printf("%d, ", ch->tab[i]);
    }
    printf("\n");
}


void libere_ch(tab* ch){
    free(ch -> tab);
    free(ch);
}




void ajout(tab* tab, int val){
    if (tab -> ind < tab -> t){
        tab -> tab[tab -> ind] = val;
        tab -> ind++;
    }
    else{ //On pourrait facilement faire un tableau dynamique si nécessaire...
        printf("Impossible\n");
    }
}

void test_rotation(void){
    tab* m = malloc(sizeof(tab));
    m->tab = malloc(sizeof(int) * 10);
    m->t = 10;
    m->ind = 7;
    m->fail = false;
    for (int i = 0; i < 7; i++){
        m->tab[i] = i;
    }

    affiche_chemin(m);
    rotation(m, 2);
    affiche_chemin(m);
    ajout(m, 8);
    affiche_chemin(m);

    libere_ch(m);
}



tab* algo(graph* g){
    int unused[g->n][g->n];
    int nb_unused[g -> n];
    for (int i = 0; i < g -> n; i++){
        nb_unused[i] = g -> degre[i];
        for (int j = 0; j < g -> degre[i]; j++){
            unused[i][j] = g -> adj[i][j];
        }
    }

    tab* ch = malloc(sizeof(tab));
    ch -> fail = false;
    ch -> tab = malloc(sizeof(int) * g -> n);
    ch -> t = g -> n;
    ch -> ind = 1;
    ch -> tab[0] = uniforme(g -> n);
    assert(ch->tab[0] < g->n);
    assert(ch->tab[0] >= 0);


    while (ch -> ind < g -> n){
        int vk = ch -> tab[ch -> ind -1];
        if (nb_unused[vk] == 0){
            ch -> fail = true;
            return ch;
        }
        else{
            int u = unused[vk][0];
            unused[vk][0] = unused[vk][nb_unused[vk] -1];
            nb_unused[vk]--;
            
            int i = 0;
            for (; i < nb_unused[u]; i++){
                if (unused[u][i] == vk){
                    unused[u][i] = unused[u][nb_unused[u] -1];
                    nb_unused[u]--;
                    i = nb_unused[u];
                }
            }
            if (i != nb_unused[u]+1){
                printf("On a pas retiré, échec\n");
                exit(1);
            }

            bool b = true;
            int j;
            for (int i = 0; i < ch->ind-1; i++){
                if (u == ch -> tab[i]){
                    b = false;
                    j = i;
                    i = ch->ind;
                }
            }
            if (b) ajout(ch, u);
            else{
                rotation(ch, j);
            }
        }
    }
    return ch;
}



int test_algo(int n, int m){
    init_alea();
    graph* g = graphe_genere_aretes(n, m);
    tab* ch = algo(g);
    while (ch->fail){
        fprintf(stderr, "Chemin trouvé, incomplet: ");
        affiche_chemin(ch);
        libere_ch(ch);
        ch = algo(g);
    }
    fprintf(stderr, "Chemin trouvé, COMPLET!! ");
    affiche_chemin(ch);
    libere(g);
    libere_ch(ch);
    return 0;
}

int main(){
    test_algo(15, 40);
}


//Cf feuille de tp pour les questions de corrections