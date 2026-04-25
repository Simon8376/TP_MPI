(** Type des données à classer *)
type data = int array

(** Type interne à ce module pour représenter ses données d'entrainement.
    Dans la partie naïve, on choisit simplement la liste des couples (donnée, étiquette).
    Dans la partie moins naïve, ce sera un arbre dimensionnel. *)
type 'label t

(** Initialisation des données d'entrainement *)
val init : (data * 'label) Seq.t -> 'label t

val most_frequent : ('a * 'b) list -> 'b 


(** Classification d'une donnée *)
val classify : (data * 'label) Seq.t -> int -> data -> 'label

