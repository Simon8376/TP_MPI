(*Question 1: il n'y a pas forcement terminaison, on peut changer les expressions a l'infini*)


(*Question 2: Aucune raison que ca soit confluent, 
Par exemple, si on n'a que l'associativité à gauche, on associe 
puis on peut jamais revenir en arrière: 
(x-x) + y -> x + (-x + y)
          -> 0 + y -> y*)


type expr = 
    | Zero
    | Var of int 
    | Plus of expr * expr 
    | Inv of expr


(*
0 + x, x
Inv(x) + x, 0
(x + y) + z, x + (y + z)
*)

exception Mismatch

type regle = expr * expr


let test_match motif f = 
  let hash = Hashtbl.create 50 in
  let rec aux motif f = 
    match motif, f with 
    |  Zero, Zero -> ()
    |  Var(x), e -> try 
                      let e' = Hashtbl.find hash x in
                      if not Anneau.expr_eq e e' then raise Mismatch 
                    with Not_found ->
                      Hashtbl.add hash x e
    |  Plus(e1, e2), Plus(f1, f2) -> test_match e1 f1; test_match e2 f2
    |  Inv(e), Inv(g) -> test_match e g 
    |  _ -> raise Mismatch 
  in
  aux motif f;
  hash


let rec reec f = 
    match f with 
    |  Zero -> Zero 
    |  Var x -> try Hashtbl.find hash x with Var x 
    |  Plus(f1, f2) -> Plus(reec f1, reec f2)
    |  Inv(f1) -> Inv(reec f1)



let rewrite_root (alpha, beta) f = 
  let hash = test_match alpha f in 
  match f, reec alpha with 
  |  Zero, Zero -> beta 
  |  Plus(f1, f2), Plus(a1, a2) |  Inv(f1), Inv(a1) |  Var x1, Var x2 -> reec beta 
  |  _ -> raise Mismatch 


let rec rewrite_somewhere (alpha, beta) f = 
  try
    rewrite_root (alpha, beta) f 
  with Mismatch -> 
    match f with 
    |  Zero | Var x -> raise Mismatch
    |  Plus(f1, f2) -> Plus(rewrite_somewhere (alpha, beta) f1, rewrite_somewhere (alpha, beta) f2)
    |  Inv(f1) -> Inv(rewrite_somewhere (alpha, beta) f1, rewrite_somewhere (alpha, beta) f2)


exception Found of expr   

let rec normalise f sys = 
  try
    List.iter (fun regle -> try 
                              let next = rewrite_somewhere regle f in 
                              let b, normalised = normalise next sys in
                              if b then raise (Found normalised)
                            with Mismatch -> ()) sys;
    f
  with Found exp -> exp


(* Inv > Plus > Zero
x = x + 0
x = 0 + x 
(x + y) + z = x + (y + z)
D'ou les règles
x + 0, x
0 + x, x
(x + y) + z, x + (y + z)
*)

let compare_rpo e1 e2 = 
  let sys = [(Var 0, Plus(Var 0, Zero)), (Var 0, Plus(Zero, Var 0)), (Plus(Plus(0, 1), 2), Plus(0, Plus(1, 2)))] in 
  let en1 = normalise e1 sys in 
  let en2 = normalise e2 sys in 
  Anneau.expr_eq en1 en2 


(*Cet ordre est bien fondé: à montrer*)








