
type grille = int array array

let _ = Random.init (int_of_float ((Sys.time ()) *. 100000000.))

let rand p = (*1 avec proba p*)
	let b = (1 lsl 30) -1 in
	let n = Random.int b in 
	if float_of_int n <= (float_of_int b) *. p then 
		1 
	else 0

let construire_grille n m p =
	let mat = Array.make_matrix n m 0 in 
	for i = 0 to n-1 do 
		for j = 0 to m-1 do 
			mat.(i).(j) <- rand (p *. float_of_int (1 - 4*((i - (n-1)/2)*(i - (n-1)/2) + (j - (m-1)/2)*(j - (m-1)/2))/((n-1)*(n-1) + (m-1)*(m-1))))
		done
	done;
	mat

let tracer_grille grille k =
  Graphics.open_graph "";
	let t = Array.length grille in
	for i = 0 to t-1 do 
		for j = 0 to t-1 do 
			let color = 
				if grille.(i).(j) = 1 then 
					Graphics.black
				else
					Graphics.white
			in
			Graphics.set_color color;
			Graphics.fill_rect (k*i) (k*j) k k
		done
	done

let reliee grille (i, j) (k, l) = 
	(grille.(i).(j) = 0) && (grille.(k).(l) = 0) && 
		((i = k && (j-l)*(j-l) = 1) || (j = l && (i-k)*(i-k) = 1))

let voisins_libres grille (i, j) = 
	let t = Array.length grille in
	let l = [(i-1, j); (i, j-1); (i+1, j); (i, j+1)] in 
	List.filter (fun (x, y) -> x >= 0 || y >= 0 || x < t || y < t || grille.(x).(y) = 0) l


exception Found


let existe_chemin grille (i, j) (k, l) = 
	let queue = Queue.create () in 
	Queue.push (i, j) queue;
	try
	while not (Queue.is_empty queue) do
		let (x, y) = Queue.pop queue in
		if (x, y) = (k, l) then raise Found 
		else 
			let l = voisins_libres grille (x, y) in 
			List.iter (fun (z, w) -> Queue.push (z, w) queue) l
	done;
	false
	with Found -> true
				
	
let existe_chemin_2 grille (i, j) = 
	let queue = Queue.create () in 
	Queue.push (i, j) queue;
	let i = ref 0 in
	let res = ref (0, 0) in
	try
	while not (Queue.is_empty queue) && !i < 1000 do
		let (x, y) = Queue.pop queue in
		res := (x, y);
		incr i;
		let l = voisins_libres grille (x, y) in 
		List.iter (fun (z, w) -> Queue.push (z, w) queue) l
	done;
	!res
	with Found -> !res


(*Pour implémenter A* il faut une file de priorité en tas binaire. 
Extraction du min en O(1)
Insertion en O(log n)*)

type pfile = {
	tab : ((int * int) * int) array; (*Couples (coordonnées), poids. C'est un tas 
									min sur le poids*)
	mutable len : int;
	loc : (((int * int) * int), int) Hashtbl.t;
}

let parent pfile i = 
	if i = 0 then pfile.tab.(0)
	else pfile.tab.(i/2)

let enfg pfile i = 
	if 2*i+1 >= pfile.len then pfile.tab.(i)
	else pfile.tab.(2*i+1)

let enfd pfile i = 
	if 2*i+2 >= pfile.len then pfile.tab.(i)
	else pfile.tab.(2*i+2)

let switch pfile i j = 
	let k = pfile.tab.(i) in 
	pfile.tab.(i) <- pfile.tab.(j);
	pfile.tab.(j) <- k;
	Hashtbl.replace pfile.loc k j;
	Hashtbl.replace pfile.loc pfile.tab.(i) i

let rec percole_bas pfile i = (*percole l'element d'indice i*)
	if snd (enfg pfile i) < snd pfile.tab.(i) then (
		switch pfile i (2*i +1);
		percole_bas pfile (2*i+1)
	)
	else if snd (enfd pfile i) < snd pfile.tab.(i) then (
		switch pfile i (2*i +2);
		percole_bas pfile (2*i+2)
	)


let rec percole_haut pfile i = 
	if snd (parent pfile i) > snd pfile.tab.(i) then (
		switch pfile i (i/2);
		percole_haut pfile (i/2))
	else i


let pfile_defile pfile = 
	if pfile.len = 0 then failwith "Vide"
	else 
		let res = pfile.tab.(0) in 
		switch pfile 0 (pfile.len -1);
		pfile.len <- pfile.len -1;
		percole_bas pfile 0;
		res 


let pfile_maj pfile el = 
	match Hashtbl.find_opt pfile.loc el with 
	|  Some v -> 
		pfile.tab.(v) <- el;
		let k = percole_haut pfile v in
		percole_bas pfile k
	|  None -> 
		pfile.tab.(pfile.len) <- el;
		pfile.len <- pfile.len +1;
		Hashtbl.add pfile.loc el (pfile.len -1);
		let _ = percole_haut pfile (pfile.len -1) in  ()

	

let draw (x, y) k color = 
	Graphics.set_color color;
	Graphics.fill_rect (k*x) (k*y) k k


let rec affiche_chemin color ch k = 
	match ch with 
	|  [] -> ()
	|  (x, y) :: t -> 
		draw (x, y) k color;
		affiche_chemin color ch k



let astar grille (i, j) (k, l) h k = 
	let h' = h (k, l) in
	let t = Array.length grille in
	let pfile = {
		tab = Array.make (t*t) ((-1, -1), -1);
		len = 0;
		loc = Hashtbl.create 40;
	} in 

	let d = Array.make_matrix t t (int_of_float infinity, []) in
	d.(i).(j) <- (0, []);
	pfile_maj pfile ((i, j), 0);

	try
		while pfile.len != 0 do 
			let (x, y), _ = pfile_defile pfile in
			draw (x, y) k Graphics.blue;
			if (x, y) = (k, l) then raise Found 
			else
			List.iter (fun (z, w) -> 
				let new_d = fst d.(x).(y) + 1 in 
				if new_d < fst d.(z).(w) then
					pfile_maj pfile ((z, w), new_d + h' (z, w));
					d.(z).(w) <- new_d, (z, w) :: snd d.(x).(y)) 
						(voisins_libres grille (x, y))
		done;
		failwith "Not found"
	with Found -> 
		affiche_chemin Graphics.red (snd d.(k).(l)) k



let eucli (x, y) (k, l) = 
	(k-x)*(k-x) + (l-y)*(l-y)



let rec find_free grille = 
	print_string "Searching...\n";
	let t = Array.length grille in
	let res = ref (Random.int t, Random.int t) in
	while grille.(fst !res).(snd !res) = 0 do
		res := Random.int t, Random.int t
	done;
	!res



let find_ch grille = 
	let (x, y) = find_free grille in 
	let k = ref 0 in 
	let l = ref 0 in
	while not ((*existe_chemin grille (x, y) (!k, !l)) && *)grille.(!k).(!l) = 0) do 
		let g = find_free grille in 
		k := fst g;
		l := snd g
	done;
	(x, y), (!k, !l)


let main () =
	let k = 5 in
	let h = eucli in
	let grille = construire_grille 100 100 0.5 in
	tracer_grille grille k;
	let (x, y) = find_free grille in
	let (k, l) = existe_chemin_2 grille (x, y) in
	draw (k, l) k Graphics.green;
	draw (x, y) k Graphics.yellow;
	astar grille (x, y) (k, l) h k;
	Unix.sleepf 100.

let _ = main ()
