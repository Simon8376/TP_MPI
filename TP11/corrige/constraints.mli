(** Représentant d'une classe d'équivalence *)
type 'a repr = { mutable rank : int ; mutable label : 'a }

(** Nœud de l'arbre union-find *)
type 'a o =
	| Repr of 'a repr
	| Ptr of int

(** Table union-find *)
type 'a t = (int, 'a o) Hashtbl.t

(** Création d'une substitution vide *)
val create : unit -> 'a t

(** Ajout d'une nouvelle variable dans une substitution.
    Cette fonction ne vérifie pas si la variable était déjà présente. *)
val add : 'a t -> int -> 'a -> unit

(** Test de présence d'une variable dans une substitution *)
val mem : 'a t -> int -> bool

(** Obtenir l'expression associée à une variable dans la substitution *)
val get_expr : 'a t -> int -> 'a

(** Ajouter l'égalité x1 = x2 = E où x1 et x2 sont des variables et E est une
    expression. Si x1 et x2 étaient auparavant associées à d'autres
    expressions, ces associations sont remplacées. *)
val merge : 'a t -> int -> int -> 'a -> unit

(** Tester si deux variables sont dans la même classe d'égalités. *)
val are_equal : 'a t -> int -> int -> bool

(** Renvoyer la substitution sous la forme d'une liste *)
val to_list : 'a t -> (int * 'a) list

(** Substituer une variable.
    On fournit en paramètre une fonction qui crée l'expression formée de la
    variable à substituer, pour les cas où la variable n'est pas présente dans
    la substitution. Si la variable est présente, on utilise l'expression
    déjà stockée dans la table. Cette fonction ne modifie pas la table. *)
val sub : 'a t -> (int -> 'a) -> int -> 'a
