type expr =
	| Zero
	| Un
	| Var of int
	| Inv of expr
	| Plus of expr * expr
	| Mult of expr * expr

let expr_eq e1 e2 =
	e1 == e2 || e1 = e2

let rec string_of_expr e =
	match e with
	| Zero -> "0"
	| Un -> "1"
	| Var(x) -> String.make 1 (Char.chr (Char.code 'a' + x))
	| Inv(e) -> "-(" ^ string_of_expr e ^ ")"
	| Plus(l, r) -> "(" ^ string_of_expr l ^ " + " ^ string_of_expr r ^ ")"
	| Mult(l, r) -> "(" ^ string_of_expr l ^ " * " ^ string_of_expr r ^ ")"

exception NotUnifiable

let genere_contraintes e1 e2 =
	let rec parcours acc e1 e2 =
		match (e1, e2) with
		| (Zero, Zero)
		| (Un, Un) -> acc
		| (Var(x), e)
		| (e, Var(x)) -> (x, e) :: acc
		| (Mult(l1, r1), Mult(l2, r2))
		| (Plus(l1, r1), Plus(l2, r2)) ->
				parcours (parcours acc l1 l2) r1 r2
		| (Inv(f1), Inv(f2)) -> parcours acc f1 f2
		| _ -> raise NotUnifiable
	in parcours [] e1 e2

(* Vérifie si une variable x apparait dans une expression e *)
let rec occurs_in x e =
	match e with
	| Var(y) -> x = y
	| Inv(e) -> occurs_in x e
	| Plus(l, r)
	| Mult(l, r) -> occurs_in x l || occurs_in x r
	| _ -> false

let unification e1 e2 =
	let subs = Constraints.create () in
	let rec traite_contraintes contraintes =
		match contraintes with
		| [] -> ()
		| (x, Var(y)) :: contraintes ->
			(* On regarde, entre les variables x et y, si elles sont dans la
			   table de hachage *)
			begin
				if x <> y then
					match (Constraints.mem subs x, Constraints.mem subs y) with
					| (false, false) ->
						Constraints.add subs x (Var(x));
						Constraints.add subs y (Var(y));
						Constraints.merge subs x y (Var(x));
						traite_contraintes contraintes
					| (true, false) ->
							Constraints.add subs y (Var(y));
							Constraints.merge subs x y (Constraints.get_expr subs x);
							traite_contraintes contraintes
					| (false, true) ->
							Constraints.add subs x (Var(x));
							Constraints.merge subs x y (Constraints.get_expr subs y);
							traite_contraintes contraintes
					| (true, true) ->
							if not (Constraints.are_equal subs x y) then
								let ex = Constraints.get_expr subs x
								and ey = Constraints.get_expr subs y in
								Constraints.merge subs x y ey;
								traite_contraintes (genere_contraintes ex ey @ contraintes)
			end
		| (x, e) :: contraintes ->
			try
				let e' = Constraints.get_expr subs x in
				traite_contraintes (genere_contraintes e e' @ contraintes)
			with Not_found ->
				Constraints.add subs x e;
				traite_contraintes contraintes
	in traite_contraintes (genere_contraintes e1 e2);
	(* On vérifie qu'aucune variable n'est une sous formule d'elle-même *)
	List.iter
		(fun (x, e) -> if e <> Var(x) && occurs_in x e then raise NotUnifiable)
		(Constraints.to_list subs);
	subs

(* Tests sur quelques exemples *)
let are_unifiable e1 e2 =
	try
		let _ = unification e1 e2 in
		true
	with
	| NotUnifiable -> false

let _ =
	let e1 = Var(0)
	and e2 = Var(1)
	in assert (are_unifiable e1 e2); Printf.fprintf stderr "ok\n"

let _ =
	let e1 = Zero
	and e2 = Un
	in assert (not (are_unifiable e1 e2)); Printf.fprintf stderr "ok\n"

let _ =
	let e1 = Mult(Var(0), Var(1))
	and e2 = Plus(Var(0), Var(1))
	in assert (not (are_unifiable e1 e2)); Printf.fprintf stderr "ok\n"

let _ =
	let e1 = Mult(Var(3), Var(4))
	in assert (are_unifiable e1 e1); Printf.fprintf stderr "ok\n"

let _ =
	let e1 = Zero
	in assert (are_unifiable e1 e1); Printf.fprintf stderr "ok\n"

let _ =
	let e1 = Un
	in assert (are_unifiable e1 e1); Printf.fprintf stderr "ok\n"

let _ =
	let e1 = Plus(Mult(Un, Var(1)), Var(2))
	and e2 = Plus(Mult(Var(0), Var(2)), Mult(Un, Zero))
	in assert (are_unifiable e1 e2); Printf.fprintf stderr "ok\n"

let _ =
	let e1 = Plus(Mult(Un, Var(1)), Var(2))
	and e2 = Plus(Var(2), Mult(Un, Zero))
	in assert (are_unifiable e1 e2); Printf.fprintf stderr "ok\n"

let _ =
	let e1 = Mult(Un, Var(0))
	and e2 = Mult(Var(0), Zero)
	in assert (not (are_unifiable e1 e2)); Printf.fprintf stderr "ok\n"

let _ =
	let e1 = Mult(Var(0), Mult(Var(1), Var(2)))
	and e2 = Mult(Plus(Var(1), Var(2)), Var(0))
	in assert (not (are_unifiable e1 e2)); Printf.fprintf stderr "ok\n"

let _ =
	let e1 = Plus(Plus(Var(3), Var(3)), Un)
	and e2 = Plus(Plus(Mult(Var(0), Var(2)), Mult(Var(2), Var(1))), Var(1))
	in assert (are_unifiable e1 e2); Printf.fprintf stderr "ok\n"

let _ =
	let e1 = Plus(Var(0), Mult(Un, Var(1)))
	and e2 = Plus(Var(1), Var(0))
	in assert (not (are_unifiable e1 e2)); Printf.fprintf stderr "ok\n"

(** Appliquer une substitution à une formule *)
let rec apply_subs subs form =
	match form with
	| Var(x) ->
			let e = Constraints.sub subs (fun c -> Var(c)) x in
			if e = Var(x)
			then Var(x)
			else apply_subs subs e
	| Inv(f) -> Inv(apply_subs subs f)
	| Mult(l,r) -> Mult(apply_subs subs l, apply_subs subs r)
	| Plus(l,r) -> Plus(apply_subs subs l, apply_subs subs r)
	| _ -> form

exception Mismatch
