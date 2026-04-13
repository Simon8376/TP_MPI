(*let classes = [|-1; -3; 1; 4; -2; 1; 2; 6|]
Il y a là 3 différentes classes (car 3 différents représentants)
Classe 1 de représentant 0, 3 de rep 1, 2 de rep 4
*)

Random.self_init ()

let rec uf_find tab v = 
  if tab.(v) < 0 then v 
  else begin
    let res = uf_find tab (tab.(v)) in 
    tab.(v) <- res;
    res 
  end 


let () = 
  let classe = [|-1; -3; 1; 4; -2; 1; 2; 6|] in 
  assert(uf_find classe 7 = 1);
  assert(classe = [|-1; -3; 1; 4; -2; 1; 1; 1|])


let uf_meme_classe tab u v =
  uf_find u tab = uf_find v tab 


let uf_union tab u v = 
  let ru = uf_find tab u in 
  let rv = uf_find tab v in 
  if tab.(ru) > tab.(rv) then 
    tab.(ru) <- rv
  else if tab.(ru) < tab.(rv) then 
    tab.(rv) <- ru
  else (
    tab.(rv) <- ru;
    tab.(ru) <- tab.(ru) -1
  )

let afficher_tab tab = 
  for i = 0 to Array.length tab -1 do
    print_int tab.(i)
  done

let () = 
  let classes = [|-1; -3; 1; 4; -2; 1; 2; 6|] in 
  uf_union classes 0 2;
  assert(classes = [|1; -3; 1; 4; -2; 1; 2; 6|])


let uf_new n = 
  Array.make n (-1)

(*
Au début chaque classe a un rang 1 et possède 1 = 2^(1-1) élément
On suppose le résultat au rang n-1 pour rk = n-1
On considère un tableau dont tous les représentants sont de rang <= n-1 et on considère 
une union qui induit un représentant de rang n. On a alors faire l'union sur deux classes de rang n-1 (cf l'algo)
La nouvelle classe a alors au moins (par HR) 2^(n-2) + 2^(n-2) = 2^(n-1) éléments ce qui vérifie le résultat
*)

(*
Complexité de uf_find: 
Soit k le rang maximal des représentants de classes et q le nombre de classes. 
Alors le nombre d'éléments du tableau est au moins q*2^(k-1) <= n donc k <= log(n/q)+1 = O(log(n))
Enfin on effectue au plus k appels à uf_find lors d'un appel (par propriété sur le rang)
donc la complexité est en O(k) = O(log(n))

Complexité de uf_union:
On effectue:
- 2 appels à uf_find
- des opérations à temps constant
Donc la complexité est en O(log(n))
*)

let voisines (x, y) n = 
  let l = ref [] in 
  let e = x mod 2 = 0 in

  if y > 0 then l := (x, y-1) :: !l;
  if y < n-1 then l := (x, y+1) :: !l;
  if e then begin
    if x > 0 && y < n-1 then l := (x-1, y) :: (x-1, y+1) :: !l
    else if x > 0  then l:= (x-1, y) :: !l;
    if x < n-1 && y < n-1 then l := (x+1, y) :: (x+1, y+1) :: !l
    else if x < n-1 then l := (x+1, y) :: !l
  end
  else begin
    if y > 0 then l := (x-1, y-1) :: (x-1, y) :: !l
    else l := (x-1, y) :: !l;
    if x < n-1 && y > 0 then l := (x+1, y-1) :: (x+1, y) :: !l
    else if x < n-1 then l := (x+1, y) :: !l
  end;
  !l

let murs n = 
  let rec f (i, j) tab = 
    match tab with 
    |  (x, y) :: t when (i > x || (i = x && j > y)) -> ((i, j), (x, y)) :: (f (i, j) t)
    |  _ :: t -> f (i, j) t 
    |  [] -> []
  in
  let l = ref [] in 
  for i = 0 to n-1 do 
    for j = 0 to n-1 do 
      l := (f (i, j) (voisines (i, j) n)) @ !l
    done;
  done;
  !l


(*La grille à n lignes et p colonnes possède (n-1 + 2n-1)*(p-1) + (n-1) = (3n-2)*(p-1)+n-1 *)

let couple_ok x y b n p = (*Teste si on est bien sur un mur intérieur*)
  not ((x = 0 && (b = 0 || b = 1)) || (x = n-1 && (b = 3 || b = 4)) || (y = 0 && x mod 2 = 1 && (b = 0 || b = 4 || b = 5)) || (y = 0 && x mod 2 = 0 && b = 5) 
    || (y = p-1 && x mod 2 = 1 && b = 2) || (y = p-1 && x mod 2 = 0 && (b = 1 || b = 2 || b = 3)))

let pos x y b = 
  let e = x mod 2 in
  let peer = match b with 
    | 0 -> (x - 1, y - e)
    | 1 -> (x -1, y - e + 1)
    | 2 -> (x, y +1)
    | 3 -> (x+1, y + 1 - e)
    | 4 -> (x+1, y - e)
    | 5 -> (x, y -1)
    | _ -> (1000, 10000)
  in
  (x, y), peer

let melange_murs murs n p = (*Marche pas trop trop smh*)
  let t = Array.length murs in 
  let res = Array.make t ((1000, 1000), (1000, 1000)) in
  let pris = Array.make_matrix n p [] in 
  for i = 0 to t -1 do
    let l, c, b = ref (Random.int (n-1)), ref (Random.int (p-1)), ref (Random.int 5) in 
    let fin = ref false in
    while (List.mem !b pris.(!l).(!c)) && not !fin do 
      if !b < 8 then incr b
      else if !l < n-1 then (
        incr l;
        b := 0
      )
      else if !c < p-1 then (
        incr c;
        l := 0;
        b := 0
      )
      else (
        c := 0;
        l := 0;
        b := 0
      )
    done;
    if couple_ok !l !c !b n p then (
      let x, y = pos !l !c !b in 
      res.(i) <- (x, y);
      pris.(!l).(!c) <- !b :: pris.(!l).(!c);
      fin := true
    )
  done;
  res

let rec afficher_tab_couples res = 
  for i = 0 to Array.length res -1 do
    let (x, y), (w, z) = res.(i) in  
    Printf.printf "((%d, %d), (%d, %d)),  " x y w z
  done

let () = 
  let murs = [|((0, 1), (1, 1)), ((0, 1), (1, 0)), ((2, 4), (2, 3)), ((2, 4), (1, 4))|] in 
  let res = melange_murs murs 6 6 in 
  afficher_tab_couples res

(*Ce mélange me paraît homogène*)


