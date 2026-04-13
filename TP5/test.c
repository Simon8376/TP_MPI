#include <stdio.h>
#include "boite_noire2.h"

int main() {
	// On fait toutes les tâches bêtement à la suite
	boite_noire *b1 = boite_cree_1();
	boite_demarre(b1, false);
	for(int i = 0; i < boite_objectif(b1); i++) {
		boite_noire_tache(b1, i);
	}
	boite_eteint(b1);
	return 0;
}
