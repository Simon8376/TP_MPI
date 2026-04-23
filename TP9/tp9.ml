(*Question 1:
c'est A ou le sommet initial c'est p et le seul final c'est q
L(A) = *)

(*Question 2:
L(0, p, q) = les mots - lettres a tq delta(p, a) = q. Si p = q alors c'est epsilon
L(n, p, q) = L(p, q)*)

(*Question 3:
L(k, p, q) = L(k-1, p, q) union ( L(k-1, p, k-1)) . L(k-1, k-1, k-1)* . L(k-1, k-1, q) ) 
*)

(*Question 4:
Soit qf dans F. On calcule la regex associée à L(n, qi, qf):
- R(L(0, p, q)) = | sur l'ensemble des mots-lettres
- R(L(k, p, q)) = R(L(k-1, p, q) | R( L(k-1, p, k) . L(k-1, k, kL(k-1, k, q) )

On fait une union des L(n, qi, qf) pour qf dans F
*)

(* Flemme *)

(*Question 6:
On a montré que Rec(A) inclu dans Reg(A)*)


type regexp = 
|  Vide 
|  Epsilon 
|  Mot of string 
|  Union of regexp * regexp 
|  Concat of regexp * regexp 
|  Etoile of regexp

let rec est_vide reg = 
  match reg with 
  |  Vide -> true
  |  Mot _ -> false 
  |  Epsilon -> false
  |  Union(r1,r2) -> est_vide r1 && est_vide r2 
  |  Concat(r1,r2) -> est_vide r1 || est_vide r2 
  |  Etoile _ -> false 


let simplifie_racine_union reg = 
  match reg with 
  |  Union(Vide, r) | Union(r, Vide) -> r 
  |  _ -> reg 

let simplifie_racine_concat reg = 
  match reg with
  |  Concat(Epsilon, r) | Concat(r, Epsilon) -> r 
  |  Concat(Vide, r) |  Concat(r, Vide) -> Vide 
  |  Concat(Mot a, Mot b) -> Mot(a ^ b)
  |  _ -> reg 

let simplifie_racine_etoile reg =
  match reg with
  |  Etoile(Vide) -> Epsilon 
  |  Etoile(Epsilon) -> Epsilon 
  |  Etoile(Etoile(r)) -> Etoile(r)
  |  _ -> reg 


let rec simplifie reg = 
  simplifie (simplifie_racine_concat (simplifie_racine_etoile (simplifie_racine_union reg)))


let () = 
(*Faire un test*) ()


type etat = int 

type transition = {
  debut : etat;
  etiq : char;
  fin : etat;
}

type automate = {
  nb_etats : int;
  initiaux : etat list;
  finaux : etat list;
  transitions : transition list;
}


let mny a = 
  let reg = Array.make_matrix a.nb_etats a.nb_etats Vide in 
  for i = 0 to a.nb_etats -1 do 
    reg.(i).(i) <- Epsilon
  done;

  let l = ref a.transitions in 
  while List.is_empty !l do
    let t = List.hd !l in 
    reg.(t.debut).(t.fin) <- Mot(String.make 1 t.etiq);
    l := List.tl !l
  done;

  for k = 1 to a.nb_etats do 
    for p = 0 to a.nb_etats -1 do 
        for q = 0 to a.nb_etats-1 do  
            reg.(p).(q) <- 
              Union(
                reg.(p).(q),
                Concat(
                  reg.(p).(k-1), 
                  Concat(
                    Etoile(reg.(k-1).(k-1)),
                    reg.(k-1).(q)
                  )
                )
              )
          done
    done
  done;

  let res = ref (if a.nb_etats = 0 then Vide else Epsilon) in 
  let li = ref a.initiaux in 
  let lf = ref a.finaux in
  while List.is_empty !lf do 
    let q = List.hd !lf in
    while List.is_empty !li do 
      let p = List.hd !li in
      res := Union(reg.(p).(q), !res);
      li := List.tl !li
    done;
    li := a.initiaux;
    lf := List.tl !lf
  done;
  simplifie !res


let rec affiche reg = 
  match reg with 
  |  Vide -> print_string "Vide"
  |  Epsilon -> print_string "Epsilon"
  |  Mot(s) -> print_string s
  |  Union(r1, r2) -> (print_string "Union(";
    affiche r1;
    print_string ", ";
    affiche r2;
    print_string ")")
  |  Concat(r1, r2) -> 
    (print_string "Concat(";
    affiche r1;
    print_string ", ";
    affiche r2;
    print_string ")")
  |  Etoile(r) -> 
    (print_string "Etoile(";
    affiche r;
    print_string ")")



let () = (*Bizarrement ça ne termine pas*)
  let auto = {
    nb_etats = 2;
    initiaux = [0];
    finaux = [1];
    transitions = [{debut = 0; etiq = 'a'; fin = 1}; {debut = 1; etiq = 'b'; fin = 0}];
  }
  in 
  let reg = mny auto in 
  affiche reg


