
type grille = int array array

let _ = Random.init (int_of_float ((Sys.time ()) *. 100000000.))


let b = 5 (*Longeur des segments de l'adjacence*)
let n, m = 200, 200 (*Taille de la grille*)
let startx, starty = n/2, m/2  (*Position de la case de départ*)
let height = 5 (*Taille de chaque pixel*)
let p = 0.3 (*Proba qu'il y ait un obstacle (grosso modo)*)
let max_iter = 1500 (*nombre d'itérations du dfs pour trouver deux points accessibles dans ensemble_accessible.
	A priori, plus max_iter est grand, plus on va cherche des points éloignés*)

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

let tracer_grille grille colo num_colo k =
  	Graphics.open_graph "";
	Graphics.resize_window 1000 1000;
	let t = Array.length grille in
	for i = 0 to t-1 do 
		for j = 0 to t-1 do 
			let color = 
				if grille.(i).(j) = 1 then 
					((1 lsl 29) * colo.(i).(j)) / num_colo
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


exception Found of int


let mix tab = 
	let arr = Array.of_list tab in 
	for i = 0 to Array.length arr -1 do 
		for j = 0 to i-1 do 
			if rand 0.5 = 1 then 
				let k = arr.(i) in 
				arr.(i) <- arr.(j);
				arr.(j) <- k 
		done;
	done;
	Array.to_list arr


let ensemble_accessibles grille (i, j) = (*modification pratique de existe_chemin*)
	let vus = Array.make_matrix (Array.length grille) (Array.length grille.(0)) false in
	let count = ref 0 in
	let s = Stack.create () in
	Stack.push (i, j) s;
	while not (Stack.is_empty s) && !count < 500 do
		incr count;
		let (x, y) = Stack.pop s in
		vus.(x).(y) <- true;
		let l = mix (voisins_libres grille (x, y)) in 
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
	mutable tab : ((int * int) * int) array; (*Couples (coordonnées), poids. C'est un tas 
									min sur le poids. Il est dynamique*)
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


let agrandit tab = 
	let n = Array.length tab in 
	let new_arr = Array.make (2*n) tab.(0) in 
	for i = 0 to n-1 do 
		new_arr.(i) <- tab.(i)
	done;
	new_arr


let pfile_maj pfile el = 
	match Hashtbl.find_opt pfile.loc el with 
	|  Some v -> 
		pfile.tab.(v) <- el;
		let k = percole_haut pfile v in
		percole_bas pfile k
	|  None -> 
		if pfile.len >= Array.length pfile.tab then 
			pfile.tab <- agrandit pfile.tab;
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
	let pfile = {
		tab = Array.make 10 ((-1, -1), -1);
		len = 0;
		loc = Hashtbl.create 40;
	} in 

	let n, m = Array.length grille, Array.length grille.(0) in

	let d = Array.make_matrix n m (10000000, []) in
	d.(i).(j) <- (0, [(i, j)]);
	pfile_maj pfile ((i, j), 0);

	try
		while pfile.len != 0 do 
			let (x, y), ch = pfile_defile pfile in
			let dist, ch = d.(x).(y) in
			affiche_chemin Graphics.blue ch height;
			if (x, y) = (k, l) then raise (Found dist)
			else begin
				List.iter (fun (z, w) -> 
					let new_d = dist + 1 in 
					if new_d < fst d.(z).(w) then begin
						pfile_maj pfile ((z, w), new_d + h1 (z, w));
						d.(z).(w) <- new_d, (z, w) :: ch;
						draw (z, w) height Graphics.yellow;
					end) 
							(voisins_libres grille (x, y));
				Unix.sleepf 0.05;
				affiche_chemin Graphics.cyan ch height
			end
		done;
		failwith "Not found"
	with Found distance -> begin
		affiche_chemin Graphics.red (snd d.(k).(l)) height;
		distance
	end


let chemin_direct (i, j) (x, y) = 
	let len_ch = max (i-x) (x-i) + max (j-y) (y-j) in
	List.init len_ch (fun c -> (i + (x-i)*c/len_ch, j + (y-j)*c/len_ch)), len_ch


let construire_graphe grille b =
	let n, m = Array.length grille, Array.length grille.(0) in 
	let adj = Array.make_matrix n m [] in (*Contient les destinations directs et leur distance*)

	let voisins_segments (i, j) = 
		for k = max 0 (i-b) to min n (i+b) do 
			for l = max 0 (j-b) to min m (j+b) do 
				let ch, len = chemin_direct (i, j) (k, l) in 
				if not (List.exists (fun (x, y) -> grille.(x).(y) = 1) ch) then 
					adj.(i).(j) <- (((k, l), len) :: adj.(i).(j))
			done
		done
	in
	for i = 0 to n-1 do 
		for j = 0 to m-1 do 
			if grille.(i).(j) = 1 then 
				adj.(i).(j) <- []
			else
				voisins_segments (i, j)
		done;
	done;
	adj



let astar_partie_3 grille adjacence (i, j) (k, l) h height = 
	let h1 = h (k, l) in
	let pfile = {
		tab = Array.make 10 ((-1, -1), -1);
		len = 0;
		loc = Hashtbl.create 40;
	} in 

	let n, m = Array.length grille, Array.length grille.(0) in


	let d = Array.make_matrix n m (10000000, []) in
	d.(i).(j) <- (0, [(i, j)]);
	pfile_maj pfile ((i, j), 0);

	try
		while pfile.len != 0 do 
			let (x, y), ch = pfile_defile pfile in
			let dist, ch = d.(x).(y) in
			affiche_chemin Graphics.blue ch height;
			if (x, y) = (k, l) then raise (Found dist) 
			else begin
				List.iter (fun ((z, w), len_ch) -> 
					let new_d = dist + len_ch in 
					if new_d < fst d.(z).(w) then begin
						pfile_maj pfile ((z, w), new_d + h1 (z, w));
						d.(z).(w) <- new_d, (fst (chemin_direct (x, y) (z, w))) @ ch;
						draw (z, w) height Graphics.yellow;
					end) 
							adjacence.(x).(y);
				Unix.sleepf 0.05;
				affiche_chemin Graphics.cyan ch height
			end
		done;
		failwith "Not found"
	with Found distance -> begin
		affiche_chemin Graphics.red (snd d.(k).(l)) height;
		distance
	end


let eucli (x, y) (k, l) = 
	(k-x)*(k-x) + (l-y)*(l-y)


let manhattan (x, y) (k, l) = 
	max ((x-k)*(x-k)) ((y-l)*(y-l))




(*Comment trouver les composantes connexes distinctes: on a un tableau vus de toutes les cases.
Par un dfs, on ajoute une nouvelle couleur en marquant tous les sommets de cette couleur à vus
Ensuite on colorie selon le marquage de vus*)


let coloration grille = 
	let n, m = Array.length grille, Array.length grille.(0) in
	let vus = Array.make_matrix n m (-1) in 
	let grille2 = Array.make_matrix n m 0 in 
	for i = 0 to n-1 do 
		for j = 0 to n-1 do 
			grille2.(i).(j) <- 1-grille.(i).(j)
		done
	done;
	
	let rec dfs k (i, j)= 
		if vus.(i).(j) = -1 then begin
			vus.(i).(j) <- k;
			List.iter (dfs k) (voisins_libres grille2 (i, j))
		end
	in

	let k = ref 0 in
	for i = 0 to n-1 do 
		for j = 0 to m-1 do 
			if vus.(i).(j) = -1 && grille.(i).(j) = 1 then (
				dfs !k (i, j);
				incr k
			)
		done;
	done;
	
	vus, !k




let main () =
	let i = ref 0 in
	while true do
		incr i;
		let h = eucli in
		let grille = construire_grille n m p in
		let colo, num_colo = coloration grille in
		let (x, y) = (startx, starty) in
		grille.(x).(y) <- 0;
		let (k, l) = choice_within (ensemble_accessibles grille (x, y)) in

		Graphics.set_window_title "astar basique";
		tracer_grille grille colo num_colo height;
		draw (k, l) height Graphics.green;
		draw (x, y) height Graphics.yellow;
		let d = astar grille (x, y) (k, l) h height in 
		Printf.printf "Iteration %d: \nNaive astar trouve une distance %d\n" !i d;
		flush stdout;

		let adj = construire_graphe grille b in
		tracer_grille grille colo num_colo height;

		Unix.sleepf 1.;
		draw (k, l) height Graphics.green;
		draw (x, y) height Graphics.yellow;
		Graphics.set_window_title "astar turbo";
		let d = astar_partie_3 grille adj (x, y) (k, l) h height in 
		Printf.printf "Segments astar trouve une distance %d\n\n" d;
		flush stdout;

		Unix.sleepf 5.
	done

let _ = main ()
