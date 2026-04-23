
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
	Graphics.resize_window 1000 1000;
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
	List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < t && y < t && grille.(x).(y) = 0) l


exception Found


let ensemble_accessibles grille (i, j) = (*modification pratique de existe_chemin*)
	let vus = Array.make_matrix (Array.length grille) (Array.length grille.(0)) false in
	let count = ref 0 in
	let s = Stack.create () in
	Stack.push (i, j) s;
	while not (Stack.is_empty s) && !count < 500 do
		incr count;
		let (x, y) = Stack.pop s in
		vus.(x).(y) <- true;
		let l = voisins_libres grille (x, y) in 
		List.iter (fun (z, w) -> if not vus.(z).(w) then Stack.push (z, w) s) l
	done;
	vus				


let rec choice_within arr = 
	let n, m = Array.length arr, Array.length arr.(0) in
	let (i, j) = Random.int n, Random.int m in 
	if arr.(i).(j) = false then choice_within arr
	else (i, j)


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
	if i = 0 then failwith "Pas de parent"
	else pfile.tab.(i/2)

let enfg pfile i = 
	if 2*i+1 >= pfile.len then failwith "Pas d'enfant gauche"
	else pfile.tab.(2*i+1)

let enfd pfile i = 
	if 2*i+2 >= pfile.len then failwith "Pas d'enfant droit"
	else pfile.tab.(2*i+2)

let switch pfile i j = 
	let k = pfile.tab.(i) in 
	pfile.tab.(i) <- pfile.tab.(j);
	pfile.tab.(j) <- k;
	Hashtbl.replace pfile.loc k j;
	Hashtbl.replace pfile.loc pfile.tab.(i) i

let rec percole_bas pfile i = (*percole l'element d'indice i*)
	if 2*i+1 >= pfile.len then ()
	else if snd (enfg pfile i) < snd pfile.tab.(i) then ( 
		switch pfile i (2*i +1);
		percole_bas pfile (2*i+1)
	)
	else if 2*i+2 >= pfile.len then ()
	else if snd (enfd pfile i) < snd pfile.tab.(i) then (
		switch pfile i (2*i +2);
		percole_bas pfile (2*i+2)
	)


let rec percole_haut pfile i =
	if i = 0 then 0 
	else if snd (parent pfile i) > snd pfile.tab.(i) then ( 
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
		Hashtbl.remove pfile.loc res;
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
		affiche_chemin color t k



let astar grille (i, j) (k, l) h height = 
	let h1 = h (k, l) in
	let n = Array.length grille in
	let m = Array.length grille.(0) in
	let pfile = {
		tab = Array.make (n*m) ((-1, -1), -1);
		len = 0;
		loc = Hashtbl.create 40;
	} in 

	let d = Array.make_matrix n m (10000000, []) in
	d.(i).(j) <- (0, [(i, j)]);
	pfile_maj pfile ((i, j), 0);

	try
		while pfile.len != 0 do 
			let (x, y), ch = pfile_defile pfile in
			let dist, ch = d.(x).(y) in
			affiche_chemin Graphics.blue ch height;
			if (x, y) = (k, l) then raise Found 
			else begin
				List.iter (fun (z, w) -> 
					let new_d = dist + 1 in 
					if new_d < fst d.(z).(w) then begin
						pfile_maj pfile ((z, w), new_d + h1 (z, w));
						Printf.printf "Added to pfile\n";
						d.(z).(w) <- new_d, (z, w) :: ch;
						draw (z, w) height Graphics.yellow;
					end) 
							(voisins_libres grille (x, y));
				Unix.sleepf 0.05;
				affiche_chemin Graphics.cyan ch height
			end
		done;
		failwith "Not found"
	with Found -> begin
		Printf.printf "FOUND!";
		affiche_chemin Graphics.red (snd d.(k).(l)) height
	end


let eucli (x, y) (k, l) = 
	(k-x)*(k-x) + (l-y)*(l-y)


let manhattan (x, y) (k, l) = 
	max ((x-k)*(x-k)) ((y-l)*(y-l))



let main () =
	let height = 5 in
	let h = manhattan in
	let grille = construire_grille 200 200 0.3 in
	tracer_grille grille height;
	let (x, y) = (100, 100) in
	grille.(x).(y) <- 0;
	let (k, l) = choice_within (ensemble_accessibles grille (x, y)) in
	draw (k, l) height Graphics.green;
	draw (x, y) height Graphics.yellow;
	astar grille (x, y) (k, l) h height;
	Unix.sleepf 1.

let _ = main ()
