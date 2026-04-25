#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

enum kleene_regex_kind {
    KLEENE_REGEX_EMPTY = 0,
    KLEENE_REGEX_EPSILON,
    KLEENE_REGEX_WORD,
    KLEENE_REGEX_STAR,
    KLEENE_REGEX_UNION,
    KLEENE_REGEX_CONCAT,
};

typedef struct kleene_regex_s {
    enum kleene_regex_kind kind;
}kleene_regex;

typedef struct kleene_regex_word_s {
    kleene_regex base;
    char *word;
}kleene_regex_word;

struct kleene_regex_binary_s {
    kleene_regex base;
    kleene_regex* left;
    kleene_regex* right;
};

typedef struct kleene_regex_binary_s kleene_regex_binary;
typedef struct kleene_regex_binary_s kleene_regex_union;
typedef struct kleene_regex_binary_s kleene_regex_concat;

typedef struct kleene_regex_unary_s {
    kleene_regex base;
    kleene_regex* sub;
}kleene_regex_star;


kleene_regex* kleene_empty(){
    kleene_regex* e = (kleene_regex*)malloc(sizeof(kleene_regex));
    if (e == NULL){
        printf("Echec");
        exit(1);
    }
    e -> kind = KLEENE_REGEX_EMPTY;
    return e;
}

kleene_regex* kleene_epsilon(){
    kleene_regex* e = (kleene_regex*)malloc(sizeof(kleene_regex));
    if (e == NULL){
        printf("Echec");
        exit(1);
    }
    e -> kind = KLEENE_REGEX_EPSILON;
    return e;
}

kleene_regex* kleene_word(char* word){
    kleene_regex_word* e = (kleene_regex_word*)malloc(sizeof(kleene_regex_word));
    if (e == NULL){
        printf("Echec");
        exit(1);
    }
    e -> base.kind = KLEENE_REGEX_WORD;
    e -> word = word;
    return (kleene_regex*)e;
}

kleene_regex* kleene_star(kleene_regex* sub){
    kleene_regex_star* e = (kleene_regex_star*)malloc(sizeof(kleene_regex_star));
    if (e == NULL){
        printf("Echec");
        exit(1);
    }
    e -> base.kind = KLEENE_REGEX_STAR;
    e -> sub = sub;
    return (kleene_regex*)e;
}

kleene_regex* kleene_union(kleene_regex* left, kleene_regex* right){
    kleene_regex_union* e = (kleene_regex_union*)malloc(sizeof(kleene_regex_union));
    if (e == NULL){
        printf("Echec");
        exit(1);
    }
    e -> base.kind = KLEENE_REGEX_UNION;
    e -> left = left;
    e -> right = right;
    return (kleene_regex*)e;
}


kleene_regex* kleene_concat(kleene_regex* left, kleene_regex* right){
    kleene_regex_concat* e = (kleene_regex_concat*)malloc(sizeof(kleene_regex_concat));
    if (e == NULL){
        printf("Echec");
        exit(1);
    }
    e -> base.kind = KLEENE_REGEX_CONCAT;
    e -> left = left;
    e -> right = right;
    return (kleene_regex*)e;
}

bool kleene_is_empty(kleene_regex* expr){
    if (expr -> kind == KLEENE_REGEX_CONCAT){
        kleene_regex_concat* y = (kleene_regex_concat*)expr;
        return kleene_is_empty((kleene_regex*)(y -> left)) || kleene_is_empty((kleene_regex*)(y -> right));
    }
    if (expr -> kind == KLEENE_REGEX_UNION){
        kleene_regex_union* y = (kleene_regex_union*)expr;
        return kleene_is_empty((kleene_regex*)(y -> left)) && kleene_is_empty((kleene_regex*)(y -> right));
    }
    if (expr -> kind == KLEENE_REGEX_STAR){
        kleene_regex_star* y = (kleene_regex_star*)expr;
        return kleene_is_empty((kleene_regex*)(y -> sub));
    }
    if (expr -> kind == KLEENE_REGEX_WORD){
        kleene_regex_word* y = (kleene_regex_word*)expr;
        return (y -> word == "");
    }
    return (expr -> kind == KLEENE_REGEX_EMPTY);
}

bool kleene_has_epsilon(kleene_regex* expr){
    if (expr -> kind == KLEENE_REGEX_CONCAT){
        kleene_regex_concat* y = (kleene_regex_concat*)expr;
        return kleene_has_epsilon((kleene_regex*)(y -> left)) && kleene_has_epsilon((kleene_regex*)(y -> right));
    }
    if (expr -> kind == KLEENE_REGEX_UNION){
        kleene_regex_union* y = (kleene_regex_union*)expr;
        return (kleene_has_epsilon((kleene_regex*)(y -> left)) && kleene_is_empty ((kleene_regex*)(y -> right))) || (kleene_has_epsilon((kleene_regex*)(y -> right)) && kleene_is_empty ((kleene_regex*)(y -> left)));
    }
    if (expr -> kind == KLEENE_REGEX_WORD){
        kleene_regex_word* y = (kleene_regex_word*)expr;
        return (y -> word == "");
    }
    return (expr -> kind == KLEENE_REGEX_EPSILON || expr -> kind == KLEENE_REGEX_STAR);
}

char* concat_tab(char* t1, char* t2){
    int l1 = strlen(t1);
    int l2 = strlen(t2);
    char* t = (char*)malloc(sizeof(char) * (l1 + l2));
    for (int i = 0; i < l1 + l2; i++){
        if (i < l1) t[i] = t1[i];
        else t[i] = t2[i-l1];
    }
    free(t1);
    free(t2);
    return t;
}


char* kleene_first_letters(kleene_regex* expr){
    if (expr -> kind == KLEENE_REGEX_CONCAT){
        kleene_regex_concat* y = (kleene_regex_concat*)expr;
        if (kleene_has_epsilon(y -> left)){
            return concat_tab(kleene_first_letters(y -> right), kleene_first_letters(y -> left));
        }
        return kleene_first_letters(y -> left);
    }
    if (expr -> kind == KLEENE_REGEX_UNION){
        kleene_regex_union* y = (kleene_regex_union*)expr;
        return concat_tab(kleene_first_letters(y -> right), kleene_first_letters(y -> left));
    }
    if (expr -> kind == KLEENE_REGEX_STAR){ 
        kleene_regex_star* y = (kleene_regex_star*)expr;
        return kleene_first_letters(y -> sub);
    }
    if (expr -> kind == KLEENE_REGEX_WORD){
        kleene_regex_word* y = (kleene_regex_word*)expr;
        char* res = (char*)malloc(sizeof(char));
        res[0] = y -> word[0];
        return res;
    }
    return "";
}

void affiche_str(char* m){
    for (int i = 0; i < strlen(m); i++){
        printf("%c", m[i]);
    }
    printf("\n");
}


//Kleene match: On vérifie que la premiere lettre de m est dans kleene_first_letters puis recursivement.

kleene_regex* retire_premiere_lettre(kleene_regex* expr){
    if (expr -> kind == KLEENE_REGEX_CONCAT){
        kleene_regex_concat* y = (kleene_regex_concat*)expr;
        return kleene_concat(retire_premiere_lettre(y -> left), y -> right);
    }
    if (expr -> kind == KLEENE_REGEX_UNION){
        kleene_regex_union* y = (kleene_regex_union*)expr;
        return kleene_union(retire_premiere_lettre(y -> left), retire_premiere_lettre(y -> right));
    }
    if (expr -> kind == KLEENE_REGEX_STAR){
        kleene_regex_star* y = (kleene_regex_star*)expr;
        return kleene_concat(retire_premiere_lettre(y -> sub), kleene_star(y -> sub)); // Pas trop sûr de celui ci
    }
    if (expr -> kind == KLEENE_REGEX_WORD){
        kleene_regex_word* y = (kleene_regex_word*)expr;
        int l = strlen(y -> word);
        printf("INTRA: %d\n", l);
        if (l >= 1){
            char* m = (char*)malloc(sizeof(char) * (l-1));
            for (int i = 1; i < l; i++){
                m[i-1] = y -> word[i];
            }
            return kleene_word(m);
        }
        else if (l == 1){
            return kleene_epsilon();
        }
        else return kleene_empty();
    }
    else return expr;
}

bool appartient(char c, char* m){
    for (int i = 0; i < strlen(m); i++){
        if (c == m[i]) return true;
    }
    return false;
}


bool kleene_match(char* m, kleene_regex* expr){
    char* premieres_lettres = kleene_first_letters(expr);
    int l = strlen(m);
    if (l != 0){
        char* m2 = (char*)malloc(sizeof(char)*(strlen(m)-1)); // Techniquement inutile mais évite le warning
        for (int i = 1; i < l; i++){
            m2[i-1] = m[i];
        }
        return appartient(m[0], premieres_lettres) && kleene_match(m2, retire_premiere_lettre(expr));
    }
    else return kleene_has_epsilon(expr);
}


int main(){
    kleene_regex* r1 = (kleene_regex*)malloc(sizeof(kleene_regex));
    r1 -> kind = KLEENE_REGEX_EPSILON;

    kleene_regex* r2 = (kleene_regex*)malloc(sizeof(kleene_regex));
    r2 -> kind = KLEENE_REGEX_EMPTY;

    kleene_regex_union* r3 = (kleene_regex_union*)malloc(sizeof(kleene_regex_union));
    r3 -> base.kind = KLEENE_REGEX_UNION;
    r3 -> left = (kleene_regex*)r1;
    r3 -> right = r2;

    kleene_regex_word* r4 = (kleene_regex_word*)malloc(sizeof(kleene_regex_word));
    r4 -> base.kind = KLEENE_REGEX_WORD;
    r4 -> word = "abc";

    kleene_regex_word* r5 = (kleene_regex_word*)malloc(sizeof(kleene_regex_word));
    r5 -> base.kind = KLEENE_REGEX_WORD;
    r5 -> word = "def";

    kleene_regex_union* r6 = (kleene_regex_union*)malloc(sizeof(kleene_regex_union));
    r6 -> base.kind = KLEENE_REGEX_UNION;
    r6 -> left = (kleene_regex*)r1;
    r6 -> right = (kleene_regex*)r4;

    kleene_regex_concat* r7 = (kleene_regex_concat*)malloc(sizeof(kleene_regex_concat));
    r7 -> base.kind = KLEENE_REGEX_CONCAT;
    r7 -> left = (kleene_regex*)r4;
    r7 -> right = (kleene_regex*)r5;

    kleene_regex_union* r8 = (kleene_regex_union*)malloc(sizeof(kleene_regex_union));
    r8 -> base.kind = KLEENE_REGEX_UNION;
    r8 -> left = (kleene_regex*)r5;
    r8 -> right = (kleene_regex*)r4;

    assert(kleene_has_epsilon((kleene_regex*)r1));
    assert(kleene_is_empty((kleene_regex*)r2));
    assert(kleene_has_epsilon((kleene_regex*)r3));
    assert(!(kleene_has_epsilon((kleene_regex*)r7)));

    affiche_str(kleene_first_letters((kleene_regex*)r5));
    affiche_str(kleene_first_letters((kleene_regex*)r8));

    kleene_regex_word* r9 = (kleene_regex_word*)(retire_premiere_lettre((kleene_regex*)r4));

    printf("%ld\n", strlen(r9 -> word));
    printf("%ld\n", strlen(r4 -> word)); // On a des choses très bizarres: le mot r4 auquel on retire la première lettre donne un mot de longueur doublée 
    affiche_str(r9 -> word);

    char* m = "abc";
    assert(kleene_match(m, (kleene_regex*)r1));

    free(r1);
    free(r2);
    free(r3);
    free(r4);
    free(r5);
    free(r6);
    free(r7);
    free(r8);
    free(r9);
}