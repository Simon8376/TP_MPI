#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/time.h>
#include <pthread.h>
#include <limits.h>
#include "boite_noire2.h"

struct boite_noire_s {
	/* Champs constants une fois l'initialisation passée. Il n'est pas
	 * nécessaire de protéger l'accès avec un mutex. */
	int objectif;
	bool strict;
	int **prerequis;
	int *duree;
	int *nb_prerequis;

	/* Les champs suivants peuvent être modifiés dans un thread, l'accès
	 * doit être protégé par le mutex global. */
	pthread_mutex_t verrou;
	int *fait;
	bool actif;
	double elapsedTime;
	struct timeval t_debut;
	struct timeval t_fin;
};

int boite_temps_estime(boite_noire *boite, int tache)
{
	return boite->duree[tache];
}

int boite_etat_tache(boite_noire *boite, int tache)
{
	if(tache < 0 || tache > boite->objectif) {
		return ERROR;
	}

	pthread_mutex_lock(&boite->verrou);
	int r = boite->fait[tache];
	pthread_mutex_unlock(&boite->verrou);
	return r;
}

int boite_objectif(boite_noire *boite)
{
	return boite->objectif;
}

int boite_nb_prerequis(boite_noire *boite, int tache)
{
	if(tache < 0 || tache > boite->objectif) {
		return ERROR;
	}

	return boite->nb_prerequis[tache];
}

int boite_prerequis_de(boite_noire *boite, int tache, int j)
{
	if(tache < 0 || tache > boite->objectif ||
			j < 0 || j > boite->nb_prerequis[tache]) {
		return ERROR;
	}

	return boite->prerequis[tache][j];
}

int boite_noire_tache(boite_noire *boite, int tache) {
	if (!boite->actif) {
		fprintf(stderr, "Boite noire non démarrée !");
		exit(1);
	}

	pthread_mutex_lock(&boite->verrou);
	for(int i = 0; i < boite->nb_prerequis[tache]; i++) {
		if(boite->fait[boite->prerequis[tache][i]] != ETAT_FINIE) {
			if(boite->strict) { 
				fprintf(stderr, "Prérequis non satisfait !\n");
				exit(1);
			}
			pthread_mutex_unlock(&boite->verrou);
			return PREREQUIS_MANQUANT;
		}
	}
	if(boite->strict && boite->fait[tache] != ETAT_PAS_COMMENCEE) {
		fprintf(stderr, "Tâche déjà faite !\n");
		exit(1);
	}
	if(boite->fait[tache] == ETAT_EN_COURS) {
		pthread_mutex_unlock(&boite->verrou);
		return EN_COURS;
	}
	if(boite->fait[tache] == ETAT_FINIE) {
		pthread_mutex_unlock(&boite->verrou);
		return DEJA_FAIT;
	}

	boite->fait[tache] = ETAT_EN_COURS;
	pthread_mutex_unlock(&boite->verrou);
	// DEBUT TACHE
	usleep(boite->duree[tache]);
	// FIN TACHE
	pthread_mutex_lock(&boite->verrou);
	boite->fait[tache] = ETAT_FINIE;
	pthread_mutex_unlock(&boite->verrou);
	return OK;
}

boite_noire *boite_nouvelle(int objectif)
{
	boite_noire *boite = malloc(sizeof(boite_noire));
	boite->objectif = 100;
	boite->strict = false;
	boite->actif = false;
	boite->prerequis = malloc(sizeof(int *) * boite->objectif);
	boite->nb_prerequis = malloc(sizeof(int) * boite->objectif);
	boite->duree = malloc(sizeof(int) * boite->objectif);
	boite->fait = malloc(sizeof(int) * boite->objectif);
	for(int id = 0; id < boite->objectif; id++) {
		boite->fait[id] = ETAT_PAS_COMMENCEE;
		boite->nb_prerequis[id] = 0;
	}
	pthread_mutex_init(&boite->verrou, NULL);
	return boite;
}

boite_noire *boite_cree_1()
{
	boite_noire *boite = boite_nouvelle(100);
	srand(42);
	for(int i = 0; i < boite->objectif; i++) {
		int duree = rand() % 1000000;
		if(i < 3) {
			duree += 5000000;
		}
		boite->duree[i] = duree;
		boite->prerequis[i] = NULL;
		boite->nb_prerequis[i] = 0;
	}
	return boite;
}

boite_noire *boite_cree_2()
{
	srand(42);
	boite_noire *boite = boite_nouvelle(1000);

	// Permutation aléatoire des numéros de tâches
	int *perm = malloc(boite->objectif * sizeof(int));
	for(int tache = 0; tache < boite->objectif; tache++) {
		int cible = rand() % (tache+1);
		perm[tache] = perm[cible];
		perm[cible] = tache;
	}

	// On alloue la place pour 2 prérequis par tâche
	for(int id = 0; id < boite->objectif; id++) {
		boite->prerequis[id] = malloc(2 * sizeof(int));
		boite->nb_prerequis[id] = 0;
		boite->fait[id] = 0 ;
	}

	// Création des dépendances et des durées
	for(int id = 0; id < boite->objectif; id+=4) {
		double f = 50000 ; // * ((double)rand())/INT_MAX ;
		double a = ((double)rand())/INT_MAX;
		double b = ((double)rand())/INT_MAX;
		boite->duree[perm[id]] = f*a;
		boite->duree[perm[id+1]] = f*b;
		boite->duree[perm[id+2]] = f*(1-a);
		boite->duree[perm[id+3]] = f*(1-b);
		if(id > 0) {
			boite->nb_prerequis[perm[id]]=2;
			boite->prerequis[perm[id]][0]=perm[id-1];
			boite->prerequis[perm[id]][1]=perm[id-2];
		}
		boite->nb_prerequis[perm[id+2]]=1;
		boite->nb_prerequis[perm[id+3]]=1;
		boite->prerequis[perm[id+2]][0]=perm[id];
		boite->prerequis[perm[id+3]][0]=perm[id+1];
	}
	free(perm);
	return boite;
}

void boite_detruit(boite_noire *boite)
{
	for(int i = 0; i < boite->objectif; i++) {
		if(boite->prerequis[i]) {
			free(boite->prerequis[i]);
		}
	}
	free(boite->prerequis);
	free(boite->nb_prerequis);
	free(boite->duree);
	free(boite->fait);
	free(boite);
}

void boite_demarre(boite_noire *boite, bool mode_strict)
{
	if(boite->actif) {
		fprintf(stderr,"Boite noire déjà démarrée !\n");
		exit(1);
	}
	boite->actif = true;
	boite->strict = mode_strict;
	gettimeofday(&boite->t_debut, NULL);
}

void boite_print_etat(boite_noire *boite)
{
	const char * trad_etat[] = {"à faire", "en cours", "finie"};
	for(int id = 0; id < boite->objectif; id++) {
		printf("Tâche %i, état %s\n", id, trad_etat[boite->fait[id]]);
		for(int j = 0; j < boite->nb_prerequis[id]; j++) {
			printf("Prérequis %d\n", boite->prerequis[id][j]);
		}
		printf("Tâche %i, état %s\n", id, trad_etat[boite->fait[id]]);
	}
}

void boite_eteint(boite_noire *boite) {
	pthread_mutex_lock(&boite->verrou);
	if(!boite->actif) {
		fprintf(stderr, "Boite noire non démarrée !\n");
		exit(1);
	}
	boite->actif = false;

	for(int i = 0; i < boite->objectif; i++) {
		if(boite->fait[i] != ETAT_FINIE) {
			fprintf(stderr, "Tâche non finie !\n");
			// boite_print_etat(boite);
			exit(1);
		}
	}
	gettimeofday(&boite->t_fin, NULL);
	boite->elapsedTime = (boite->t_fin.tv_sec - boite->t_debut.tv_sec) * 1000.0;
	boite->elapsedTime += (boite->t_fin.tv_usec - boite->t_debut.tv_usec) / 1000.0;
	printf("Temps total %lf\n", boite->elapsedTime);
	pthread_mutex_unlock(&boite->verrou);
}
