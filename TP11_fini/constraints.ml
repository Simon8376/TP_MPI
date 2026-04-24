(** Représentant d'une classe d'équivalence *)
type 'a repr = { mutable rank : int ; mutable label : 'a }

(** Nœud de l'arbre union-find *)
type 'a o =
	| Repr of 'a repr
	| Ptr of int

(** Table union-find *)
type 'a t = (int, 'a o) Hashtbl.t

(** Création d'une substitution vide *)
let create () =
	Hashtbl.create 42

let add table x expr =
    Hashtbl.add table x (Repr({rank = 0; label = expr}))

let mem table x =
	Hashtbl.mem table x

let rec find_repr table x =
	match Hashtbl.find table x with
	| Repr(r) -> (x, r)
	| Ptr(y) ->
		let (z, r) = find_repr table y in
		Hashtbl.replace table x (Ptr(z));
		(z, r)

let get_expr table x =
	(snd (find_repr table x)).label

let merge table x1 x2 e =
	let (repr1, l1) = find_repr table x1
	and (repr2, l2) = find_repr table x2 in
	assert (repr1 <> repr2);
	if l2.rank < l1.rank then begin
		l1.label <- e;
		Hashtbl.replace table repr2 (Ptr(repr1))
	end
	else begin
		l2.rank <- if l1.rank = l2.rank then 1 + l2.rank else l2.rank;
		l2.label <- e;
		Hashtbl.replace table repr1 (Ptr(repr2))
	end

let are_equal table x1 x2 =
	let (r1, _) = find_repr table x1
	and (r2, _) = find_repr table x2 in
	r1 = r2

let to_list table =
	Hashtbl.fold (fun x r acc ->
		(x, get_expr table x) :: acc) table []

let sub table f x =
	try get_expr table x
	with Not_found -> f x
