#ifndef __BOITE_NOIRE2_H
#define __BOITE_NOIRE2_H

#include <stdbool.h>

/**
 * Type des boites noires.
 * On ne sait pas ce que contient une boite noire, le type est opaque. Les
 * boites noires doivent être manipulées avec les fonctions fournies par la
 * suite.
 */
struct boite_noire_s;
typedef struct boite_noire_s boite_noire;

/**
 * Boites noires à traiter
 */
boite_noire *boite_cree_1();
boite_noire *boite_cree_2();

/**
 * Démarrage d'une boite noire.
 * En mode strict, il est interdit de tenter d'exécuter une tâche si ses
 * prérequis ne sont pas terminés : si on le fait, le programme sera arrêté
 * immédiatement. En mode non strict, la fonction "boite_noire" se contentera
 * de signaler, par sa valeur de retour, qu'il manque des prérequis pour la
 * tâche.
 */
void boite_demarre(boite_noire *boite, bool mode_strict);

/**
 * Renvoie le nombre total de tâches que la boite noire doit exécuter.
 */
int boite_objectif(boite_noire *boite);

/**
 * Extinction d'une boite noire.
 * Elle valide le fait que toutes les tâches ont bien été exécutées.
 * Si c'est le cas, elle renvoie le temps écoulé depuis l'allumage de la boite,
 * sinon elle renvoie -1 si la boite a été éteinte alors que toutes les tâches
 * n'ont pas été effectuées.
 */
void boite_eteint(boite_noire *boite);

/**
 * Destruction d'une boite noire.
 * La mémoire est libérée, il n'est plus possible de se servir de la boite par
 * la suite.
 */
void boite_detruit(boite_noire *boite);

/**
 * Exécution d'une tâche, donnée par son numéro, par la boite noire.
 * Cette fonction renvoie une des constantes définies ci-dessous.
 */
int boite_noire_tache(boite_noire *boite, int tache);

#define PREREQUIS_MANQUANT -3
#define DEJA_FAIT -2
#define EN_COURS -1
#define OK 0
// La valeur ERROR ne devrait être renvoyée qu'en cas de paramètre absurde
#define ERROR 42

/**
 * Renvoie le temps, approximatif, qui sera nécessaire pour exécuter une tâche.
 */
int boite_temps_estime(boite_noire *boite, int tache);

/**
 * Renvoie le nombre de prérequis d'une tâche.
 */
int boite_nb_prerequis(boite_noire *boite, int tache);

/**
 * Renvoie l'identifiant de tâche qui correspond au j-ème prérequis de "tache".
 * On suppose que 0 <= j < nb_prerequis(tache)
 */
int boite_prerequis_de(boite_noire *boite, int tache, int j);

/**
 * Fonction qui renvoie l'état d'une tâche donnée.
 * La valeur renvoyée est l'une des constantes définies ci-dessous
 */
int boite_etat_tache(boite_noire *boite, int tache);
#define ETAT_PAS_COMMENCEE 0
#define ETAT_EN_COURS (- EN_COURS)
#define ETAT_FINIE (- DEJA_FAIT)

/**
 * Affichage de l'état.
 * Fonction qui n'est pas thread-safe.
 */
void boite_print_etat(boite_noire *boite);

#endif /* __BOITE_NOIRE2_H */
