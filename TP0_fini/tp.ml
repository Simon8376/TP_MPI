

type joueur =
  | Alice
  | Bob

type coord = int * int

type barriere = {
  horizontal : bool;
  pos : coord
}

type plateau = {
  dim : int;
  alice : coord;
  bob : coord;
  barrieres : barriere list;
  stock_alice : int;
  stock_bob : int;
  tour : joueur
}

(*Question 1: A qui le tour*)


let gagnant p = 
  let x, y = p.alice in 
  let z, w = p.bob in
  if y = p.dim -1 then Some Alice 
  else if z = p.dim -1 then Some Bob
  else None

let p0 = {
  dim = 4;
  alice = 2, 2;
  bob = 3, 2;
  barrieres = [{horizontal = true; pos = (1,1)}; {horizontal = false; pos = (0,0)}];
  stock_alice = 5;
  stock_bob = 5;
  tour = Bob
}

let p1 = {
  dim = 4;
  alice = 2, 2;
  bob = 2, 1;
  barrieres = [{horizontal = true; pos = (1,1)}; {horizontal = false; pos = (0,0)}];
  stock_alice = 5;
  stock_bob = 5;
  tour = Bob
}

let () = 
  assert(gagnant p0 = Some Bob)


let cases_separees c1 c2 b =
  let x1, y1 = c1 in 
  let x2, y2 = c2 in
  let x3, y3 = b.pos in
  if b.horizontal then
    y1 = y2 && y1 = y3 && ((x1 = x3 - 1 && x2 = x3 + 1) || (x1 = x3 + 1 && x2 = x3 - 1))
  else 
    (x1 = x2) && (x1 = x3) && (((y1 = y3 - 1) && (y2 = y3 + 1)) || ((y1 = y3 + 1) && (y2 = y3 - 1)))

let b = {
  horizontal = true;
  pos = (2, 1)
}

let () = 
    assert(cases_separees (1,1) (3,1) b = true);
    assert(cases_separees (1,1) (4,1) b = false)

let rec appartient (x,y) l =
  match l with 
  |  [] -> 0
  |  b :: t when (x,y) = b.pos -> if b.horizontal then 1 else (-1)
  |  b :: t -> appartient (x,y) t

let affiche_plateau p =
  for x = 0 to 2*(p.dim) do 
    for y = 0 to 2*(p.dim) do 
      if x mod 2 = 1 && y mod 2 = 1 then 
        if (x/2, y/2) = p.alice then print_string " A"
        else if (x/2, y/2) = p.bob then print_string " B"
        else print_string "  "
      else if x mod 2 = 0 && y mod 2 = 0 then (
        if appartient (x/2, y/2 -1) p.barrieres = 1 then print_char '_'
        else if appartient (x/2 -1,y/2) p.barrieres = -1 then print_char '|'
        else print_char '+';
        print_char ' ')
      else print_char ' '
    done;
    print_newline ()
  done;
  print_newline();
  print_newline()

let () = affiche_plateau p1


let cases_voisines p (x, y) = (*Oublié cas ou il y a un joueur sur une case a cote*)
  let possible = Array.make 4 true in 
  let rec parcours_barrieres l =
    match l with
    |  [] -> ()
    |  {horizontal = z; pos = xb,yb} :: t ->
      if possible.(0) && z && x = xb && (y = yb || y = yb +1) then
        possible.(0) <- false
      else if possible.(2) && z && x = xb -1 && (y = yb || y = yb +1) then 
        possible.(2) <- false
      else if possible.(1) && not z && y = yb -1 && (x = xb +1 || x = xb) then
        possible.(1) <- false
      else if possible.(3) && not z && y = yb && (x = xb +1 || x = xb) then 
        possible.(3) <- false;
      parcours_barrieres t 
  in 
  parcours_barrieres p.barrieres;
  let cases = ref [] in 
  if possible.(0) && x -1 >= 0 then cases := (x-1, y) :: !cases;
  if possible.(1) && y+1 <= p.dim -1 then cases := (x, y+1) :: !cases;
  if possible.(2) && x+1 <= p.dim -1 then cases := (x+1, y) :: !cases;
  if possible.(3) && y -1 >= 0 then cases := (x, y-1) :: !cases;
  !cases 

let () = 
  assert(cases_voisines p1 (1, 1) = [(1, 0); (2, 1); (1, 2)]);
  assert(cases_voisines p1 (0, 3) = [(0, 2); (1, 3)])

exception Gain

let peut_gagner j p =
  let vus = Array.make_matrix p.dim p.dim false in
  let rec parcours_gain (x, y) =
    if not vus.(x).(y) then begin
      vus.(x).(y) <- true;
      if (j = Alice && y >= p.dim -1) || (j = Bob && x >= p.dim -1) then true
      else List.fold_left (fun b (i, j) -> parcours_gain (i, j) || b) false (cases_voisines p (x, y))
    end
    else false
  in
  parcours_gain (if j = Alice then p.alice else p.bob)

let p2 = {
  dim = 4;
  alice = 0, 0;
  bob = 2, 2;
  barrieres = [{horizontal = true; pos = (1,1)}; {horizontal = false; pos = (0,1)}; {horizontal = true; pos = (2, 0)}];
  stock_alice = 5;
  stock_bob = 5;
  tour = Bob
}

let () = 
  print_newline ();
  print_newline ();
  affiche_plateau p2;
  assert(peut_gagner Alice p1);
  assert(not(peut_gagner Alice p2));
  assert(peut_gagner Bob p1)

exception Crac

let barriere_possible b p =
  let xb, yb = b.pos in
  let rec chevauchement l = 
    match l with
    | [] -> ()
    | h :: t -> 
      let x, y = h.pos in
      if not b.horizontal && y = yb && not h.horizontal then 
        if x = xb -1 || x = xb+1 || x = xb then raise Crac;
      if b.horizontal && x = xb && h.horizontal then 
        if y = yb -1 || y = yb+1 || x = xb then raise Crac;
      if b.horizontal && not h.horizontal then
        if (xb = x -1 && yb = y +1) then raise Crac;
      if h.horizontal && not b.horizontal then 
        if (xb = x+1 && yb = y-1) then raise Crac;
      chevauchement t
  in
  try
    chevauchement p.barrieres;
    not((b.horizontal && (xb <= 0 || yb >= p.dim -1 || yb < 0 || xb > p.dim -1)) || (not b.horizontal && (xb < 0 || yb > p.dim -1 || yb <= 0 || xb >= p.dim -1))) 
      && (
        let ptemp = {
          dim = p.dim;
          alice = p.alice;
          bob = p.bob;
          barrieres = b :: p.barrieres;
          stock_alice = p.stock_alice;
          stock_bob = p.stock_bob;
          tour = p.tour
          } in 
        (peut_gagner Alice ptemp) && (peut_gagner Bob ptemp))
  with Crac -> false
    

let () = 
  assert(not (barriere_possible {horizontal = true; pos = (1,0)} p1));
  assert(barriere_possible {horizontal = false; pos = (1,1)} p1)



let coups_possibles p =
  let next_j = if p.tour = Alice then Bob else Alice in
  let coups = ref (List.map (fun pos -> {dim = p.dim; 
                                        alice = if p.tour = Alice && pos != p.bob then pos else p.alice;
                                        bob = if p.tour = Bob && pos != p.alice then pos else p.bob;
                                        barrieres = p.barrieres;
                                        stock_alice = p.stock_alice;
                                        stock_bob = p.stock_bob;
                                        tour = next_j}) (cases_voisines p (if p.tour = Alice then p.alice else p.bob))) in 
  List.iter (fun pos -> if pos = p.alice || pos = p.bob then          (*On ajoute les cases de saut*)
      List.iter (fun pos -> coups := {dim = p.dim; 
                            alice = if p.tour = Alice && pos != p.bob then pos else p.alice;
                            bob = if p.tour = Bob && pos != p.alice then pos else p.bob;
                            barrieres = p.barrieres;
                            stock_alice = p.stock_alice;
                            stock_bob = p.stock_bob;
                            tour = next_j} :: !coups) (cases_voisines p (if p.tour = Alice then p.bob else p.alice))) (cases_voisines p (if p.tour = Alice then p.alice else p.bob));
  let ajoute barriere joueur_bouge c = 
      c := {dim = p.dim; 
        alice = p.alice;
        bob = p.bob;
        barrieres = barriere :: p.barrieres;
        stock_alice = if joueur_bouge = Alice then p.stock_alice -1 else p.stock_alice;
        stock_bob = if joueur_bouge = Bob then p.stock_bob -1 else p.stock_bob;
        tour = next_j} :: !c;
  in
  for i = 0 to p.dim -1 do 
    for j = 0 to p.dim -1 do 
      let b1 = {horizontal = true; pos = (i, j)} in
      let b2 = {horizontal = false; pos = (i, j)} in
      let v1, v2 = (barriere_possible b1 p), (barriere_possible b2 p) in
      if  (p.tour = Bob && p.stock_bob > 0) || (p.tour = Alice && p.stock_alice > 0) then 
        (if v1 then ajoute b1 p.tour coups;
        if v2 then ajoute b2 p.tour coups)
    done;
  done;
  !coups 

exception Trouve of coord list


let plus_court_chemin (x,y) f p = 
  let file = Queue.create () in 
  let vus = Array.make_matrix p.dim p.dim false in
  Queue.push [(x,y)] file;
  try
    while not (Queue.is_empty file) do 
      match Queue.take file with
      | (i, j) :: t -> 
        if f (i, j) then raise (Trouve ((i, j) :: t))
        else List.iter (fun (k, l) -> if not vus.(k).(l) then (vus.(k).(l) <- true; Queue.push ((k, l) :: (i, j) :: t) file)) (cases_voisines p (i, j))
      | [] -> ()
    done;
    []
  with Trouve l -> List.rev l 

let rec affiche_liste l = 
  match l with
  |  [] -> print_newline ()
  |  (x, y) :: t -> Printf.printf "(%d, %d) " x y; affiche_liste t 

let () = 
  let gain (x, y) = x = 0 || x = p1.dim -1 || y = 0 || y = p1.dim-1 in 
  assert(plus_court_chemin (2, 2) gain p1 = [(2, 2); (3, 2)])


let strategie_gloutonne p =
  let gaina (x, y) = y = p.dim -1  in
  let gainb (x, y) = x = p.dim -1 in
  let la = plus_court_chemin (p.alice) gaina p in 
  affiche_liste la;
  let lb = plus_court_chemin (p.bob) gainb p in 
  affiche_liste lb;
  let long = let lla = List.length la in let llb = List.length lb in if lla > llb then -1 else if llb > lla then 1 else 0 in
  let e = ref 0 in

  let positionnement_barriere l = (*Choix de quelle barriere poser au lieu de jouer*) (*Marche pas bien si le joueur va vers la gauche ou vers le haut*)
    let rec decidons_ou l (xprev, yprev)=
      match l with
      | [] -> None
      | (x, y) :: t -> let b = {horizontal = yprev = y; pos = (x, y)} in if barriere_possible b p then (e := 1; Some b) else decidons_ou t (x, y)
    in 
    (*let lsec = List.map (fun (x, y) -> if horizontal then (x, y-1) else (x-1, y)) (List.tl l) in  (*On construit une liste par ordre d'importance croissante avec les deux files de barrieres possibles*)
    let rec lcat l1 l2 = match l1 with | h :: t -> h :: lcat l2 l1 | [] -> l2 in 
    decidons_ou (lcat (List.tl l) lsec)*)
    decidons_ou (List.tl l) (List.hd l)
  in

  let bar_hyp_alice = positionnement_barriere lb in
  let bar_hyp_bob = positionnement_barriere la in

  Printf.printf "Tour: %s; Plus Court: %s; stock_bob: %d\n" (if p.tour = Alice then "alice" else "bob") (if long = 1 then "alice" else if long = -1 then "bob" else "none") p.stock_bob;
  {dim = p.dim; 
  alice = if p.tour = Alice && (long != -1 || bar_hyp_alice = None) then 
            (List.hd (List.tl la)) 
          else p.alice;
  bob = if p.tour = Bob && (long != 1 || bar_hyp_bob = None) then 
          (List.hd (List.tl lb)) 
        else p.bob;
  barrieres = (match p.tour, long, bar_hyp_alice, bar_hyp_bob with
                | Alice, x, Some b, _  when x = -1 -> b :: p.barrieres
                | Bob, x, _, Some b when x = 1 -> b :: p.barrieres
                | _ -> p.barrieres);
  stock_alice = if p.tour = Alice then p.stock_alice - !e else p.stock_alice;
  stock_bob = if p.tour = Bob then p.stock_bob - !e else p.stock_bob;
  tour = if p.tour = Alice then Bob else Alice}


let p3 = {
  dim = 5;
  alice = 2, 0;
  bob = 0, 2;
  barrieres = [];
  stock_alice = 10;
  stock_bob = 10;
  tour = Alice
}

let () = 
    let p = ref p3 in 
    while gagnant !p = None do 
      affiche_plateau !p;
      p := strategie_gloutonne !p
    done


let minmax p = (*Ca marche pas jsp pk*)
  let vus = Hashtbl.create 10000 in 


  let minimum (d1, v1) (d2, v2) p = (*Minimum prioritaire sur v, puis minimisant la distance en cas d'ég si c'est dans le bon sens, maximise sinon*)
    if v1 > v2 || (v1 = v2 && ((d1 < 20 && v1 = -1 && d1 > d2) || (d2 < 20 &&v1 = 1 && d1 < d2))) then d2, v2
    else d1, v1
  in

  let maximum (d1, v1) (d2, v2) p = (*Maximum prioritaire sur v, puis minimisant la distance en cas d'ég*)
    if v1 > v2 || (v1 = v2 && ((d2 < 20 && v1 = 1 && d1 < d2) || (d1 < 20 && v1 = -1 && d1 > d2))) then d1, v1
    else d2, v2
  in

  let rec valeur p h = (*Renvoie la valeur d'attraction de p si elle est définie, sinon elle est construite via parcours puis renvoyée*)
    match Hashtbl.find_opt vus p with
    |  None -> parcours p (h+1); valeur p h
    |  Some (d, v) -> d, v

  and parcours p1 h =
    let _, y = p1.alice in
    let i, _ = p1.bob in
    if y = p1.dim -1 then Hashtbl.add vus p1 (0, 1)
    else if i = p1.dim -1 then Hashtbl.add vus p1 (0, -1)
    else if h >= 10 then Hashtbl.add vus p1 (100, 0)
    else 
      let c = coups_possibles p1 in
      let dist, valeur = List.fold_left (fun acc p2 -> 
                        if p2.tour = Alice then maximum acc (valeur p2 h) p1
                        else minimum acc (valeur p2 h) p1) (valeur (List.hd c) h) (List.tl c) in
      Hashtbl.add vus p1 (dist +1, valeur)


  in
  parcours p 0;
  vus
    

let traitement_min_max p  vus =
  let rec parcours_coups c = (*Renvoie un coup possible et un entier k. Plus k est proche de 0, plus Alice/Bob est proche de gagner*)
    match c with
    | [] -> 0, 0, None
    | p1 :: t -> match Hashtbl.find_opt vus p1 with
                  |  None -> failwith "N'arrive pas"
                  |  Some (d, v) -> let d', v', p = parcours_coups t in 
                                  if (p1.tour = Alice && (v > v' || (d' < 20 && v = v' && v = 1 && d <= d')) || (p1.tour = Bob && (v < v' || (d' < 20 && v = v' && v = -1 && d <= d')))) || p = None then d, v, Some p1 
                                  else d', v', p
  in
  let d, v, op = parcours_coups (coups_possibles p) in 
  print_int d;
  match op with
  | None -> failwith "N'arrive jamais"
  | Some plat -> plat

let p4 = {
  dim = 3;
  alice = 1, 0;
  bob = 0, 1;
  barrieres = [];
  stock_alice = 10;
  stock_bob = 10;
  tour = Alice
}


let () = 
  Printf.printf "\nJEU AVEC MINMAX\n";
  let p = ref p4 in 
  let vus = ref (minmax !p) in
  let n = ref 0 in
  while gagnant !p = None && !n < 10 do 
    affiche_plateau !p;
    p := traitement_min_max !p !vus;
    vus := minmax !p
  done;
  affiche_plateau !p


let alphabeta p = (*BIIIG flemme: comprends exactement ce que tu dois faire*)
  let vus = Hashtbl.create 1000 in 

  let minimum (d1, v1) (d2, v2) p = (*Minimum prioritaire sur v, puis minimisant la distance en cas d'ég si c'est dans le bon sens, maximise sinon*)
    if v1 > v2 || (v1 = v2 && ((d1 < 20 && v1 = -1 && d1 > d2) || (d2 < 20 &&v1 = 1 && d1 < d2))) then d2, v2
    else d1, v1
  in

  let maximum (d1, v1) (d2, v2) p = (*Maximum prioritaire sur v, puis minimisant la distance en cas d'ég*)
    if v1 > v2 || (v1 = v2 && ((d2 < 20 && v1 = 1 && d1 < d2) || (d1 < 20 && v1 = -1 && d1 > d2))) then d1, v1
    else d2, v2
  in

  let calcul_val p h c a b =
    match c with
    |  [] -> None 
    |  p1 :: t -> match Hashtbl.find_opt p1 with
                  |  None -> parcours p1 (h+1) b None; if bHashtbl.find p1 
                  |  Some v -> 

  let rec valeur p h = (*Renvoie la valeur d'attraction de p si elle est définie, sinon elle est construite via parcours puis renvoyée*)
    match Hashtbl.find_opt vus p with
    |  None -> parcours p (h+1); valeur p h
    |  Some (d, v) -> d, v

  and parcours p1 h a b =
    let _, y = p1.alice in
    let i, _ = p1.bob in
    if y = p1.dim -1 then Hashtbl.add vus p1 (0, 1)
    else if i = p1.dim -1 then Hashtbl.add vus p1 (0, -1)
    else if h >= 10 then Hashtbl.add vus p1 (100, 0)
    else
      let c = coups_possibles p in 
      let dist, valeur = calcul_val p h c a b in
      Hashtbl.add vus p1 (dist, valeur)
