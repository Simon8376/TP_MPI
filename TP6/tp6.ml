type litteral = 
  |  X of int 
  |  NonX of int 


type fnc = litteral list list 

let rec affiche_liste l = 
  match l with 
  |  [] -> ()
  |  h :: t -> Printf.printf "%s, " h; affiche_liste t



let rec affiche_clause clause = 
  match clause with 
  |  [] -> ()
  |  X(n) :: t -> Printf.printf "X %d, " n; affiche_clause t
  | NonX(n) :: t -> Printf.printf "NonX %d, " n; affiche_clause t



let rec affiche_formule l = 
  match l with 
  |  [] -> ()
  |  clause :: t -> 
    affiche_clause clause;
    print_newline ();
    affiche_formule t




let traitement a f l next_var =
    let hash = Hashtbl.create (int_of_string a) in
    let rec cree_clause liste = 
      match liste with 
      |  ["0"] -> []
      |  [] -> []
      |  s :: tail -> 
        let s2 = 
          if s.[0] = '-' then 
            String.sub s 1 (String.length s -1) 
        else 
          s 
        in
        (*Printf.printf "__%s__\n" s2;*)
        let num =
          match Hashtbl.find_opt hash s2 with 
          | Some num -> num
          | None ->
            Hashtbl.add hash s2 !next_var;
            incr next_var;
            !next_var - 1
        in
        (if s.[0] != '-' then X(num) else NonX(num)) :: cree_clause tail
    in


    try 
      while true do 
        let line = input_line f in (*craque au bout d'un moment*)
        if line != "" && line.[0] != '#' then
          let liste = String.split_on_char ' ' line in 
          let t = cree_clause liste in
          l := t :: !l
      done;
      !l
    with End_of_file -> !l


let lire_dimacs nom = 
  let f = open_in nom in 
  let l = ref [] in 
  let s = input_line f in
  let s = String.split_on_char ' ' s in 
  let next_var = ref 1 in

  match s with
  |  ["p"; "cnf"; a; b] -> 
    traitement a f l next_var
  | _ -> failwith "Erreur au debut"

  
    
(*
let () = 
  let l = lire_dimacs "test_dimacs.txt" in 
  affiche_formule l
  *)


let rec simplifie_clause clause hash = 
  match clause with 
  |  [] -> true, []
  |  litt :: t -> 
    let b, reste = simplifie_clause t hash in 
    if not b then 
      false, []
    else 
      let e, v = 
        match litt with 
        |  X(n) -> 1, n
        |  NonX(n) -> -1, n 
      in
      match Hashtbl.find_opt hash v with 
      |  None -> (*On a pas encore croisé ce littéral dans la clause*)
        (Hashtbl.add hash v e; true, litt :: reste)
      |  Some autre_e when autre_e = e -> (*On a un meme litteral deux fois dans une clause*)
        true, reste 
      |  Some autre_e -> (*On a deux littéraux de polarité opposée*)
        false, []
    

  
(*
let rec calcul_length formule = 
  match formule with 
  |  [] -> 0
  |  h :: t -> List.length h + calcul_length t*)


let rec simplifie_antagonistes formule = 
  match formule with 
  |  clause :: t -> 
    let hash = Hashtbl.create (List.length clause) in
    let _, simple = simplifie_clause clause hash in 
    if simple = [] then simplifie_antagonistes t 
    else simple :: simplifie_antagonistes t 
  |  [] -> []



let () = 
    let formule = [[X(2); X(2); X(4); X(1)]; [X(1); NonX(2); NonX(3)]; [X(1); NonX(1)]] in 
    let formule_simplifie = simplifie_antagonistes formule in 
    assert(formule_simplifie = [[X(2); X(4); X(1)]; [X(1); NonX(2); NonX(3)]])




(*Question 4
Soit s satisfaisant F'. On pose s| la valuation égale à s pour toutes valeurs de V\p et vrai pour p si l = p et faux si l = non(p)
Alors
- la clause unitaire (l) est vraie par s|
- Soit C une clause de F1. C contient l qui est vrai selon s| donc C est vraie selon s| donc F1 également
- Les variables de F3 sont dans V\p donc s| sur F3 = s sur F3 donc F3 vraie selon s| ssi F3 vraie selon s
- Soit C une clause de F2. F2' ou non(l) = F2 donc blabla ok 

Soit s| satisfaisant F. on a nécessairement s|(l) = vrai
- F2' ou non(l) = F2 donc s|(F2') = s|(F2) donc ok 
- F3 ne contient pas l donc meme argu*)


(*Question 5
x1 et (x4 ou x6 ou x7) et (non(x1) ou non(x6))  =>  (x4 ou x6 ou x7) ou (non(x6))   =>    (x4 ou x7)
                                            (x1 est isolé)                    (non(x6) est isolé)*)    

let rec max_clause clause = 
  match clause with 
  |  [] -> failwith "Le max sur une liste vide n'existe pas"
  |  [m] -> m 
  |  h :: t -> max h (max_clause t)

let nouveau_lit_isole formule =  (*C'est un peu embetant vu que cette fonction est linéaire en le nombre de termes total*)
  let rec aux formule = 
    (*Renvoie quelque chose de bien*)
    match formule with 
    |  [] -> None
    |  [litt_isole] :: t -> Some litt_isole 
    |  clause :: t -> aux t 
  in 
  aux formule

  

let rec simplification litt formule = 

  let meme_variable l1 l2 = 
    match l1, l2 with 
    |  X(n), X(m) | X(n), NonX(m) | NonX(n), X(m) | NonX(n), NonX(m) -> n = m 
  in

  let rec simplif_clauses litt clause = 
    match clause with 
    |  [] -> []
    |  h :: t when h = litt -> (*la clause contient le litt: on peut la retirer*)
      []
    |  h :: t when meme_variable h litt -> (*la clause contient non(litt): on peut retirer h*)
      simplif_clauses litt t 
    |  h :: t -> (*Aucune correspondance*)
      h :: simplif_clauses litt t 
  in 

  match formule with 
  |  [] -> []
  |  clause :: t -> 
    let simple = simplif_clauses litt clause in 
    if simple = [] then 
      simplification litt t 
    else
      simple :: simplification litt t 



let rec propagation formule  = 
  match nouveau_lit_isole formule with 
  |  None -> formule 
  |  Some litt -> 
    propagation (simplification litt formule)



let () = 
  let f = [[X(1)]; [X(4); X(6); X(7)]; [NonX(1); NonX(6)]] in 
  assert(propagation f = [[X(4); X(7)]])


(*
- nouveau_lit_isole marche lineairement au nombre de clauses
- simplification marche lineairement à la taille de F (on parcourt toute la formule)
- propagation fait au plus n appels à simplification (on enlève un litt à chaque fois)
donc en O(n²) *)

