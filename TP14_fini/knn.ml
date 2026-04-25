type data = int array

type 'a arbre = Nil of data * 'a | Noeud of 'a arbre * int * int * 'a arbre (*Enfant gauche, distance à la médiane, nombre de descendants, enfant droit*)

type 'label t = (data * 'label) list

let init seq =
	List.of_seq seq



 (*Compte le nb d'occurrences de x dans l*)
let rec count_item x l = 
	match l with 
	|  [] -> 0
	|  y :: t -> (if snd y = snd x then 1 else 0) + count_item x t 


let most_frequent l = 
	if l = [] then failwith "Empty"
	else
	let biggest = ref (snd (List.hd l)) in
	if List.fold_left (fun acc x -> 
						let n = count_item x l in 
						if n > acc then begin 
							biggest := snd x;
							n 
						end
						else acc) (-1) l = -1 then 
		failwith "N'arrive pas normalement"
	else !biggest 



let euclidean_dist arr1 arr2 = 
	let res = ref 0 in 
	for i = 0 to Array.length arr1 -1 do 
		res := !res + (arr1.(i)-arr2.(i)) * (arr1.(i) - arr2.(i))
	done;
	sqrt (float_of_int !res)



let k_closest seq k data = 
	let queue = ref (PrioQueue.create (fun (a, _) (b, _) -> Float.compare b a)) in
	let rec insertion s =
		match s () with 
		|  Seq.Nil -> ()
		|  Seq.Cons((im, res), s2) -> 
			let new_d = euclidean_dist im data in
			let t = PrioQueue.size !queue in
			if t = k && new_d < fst (PrioQueue.top !queue) then (
				queue := PrioQueue.remove_top !queue;
				queue := PrioQueue.insert !queue (new_d, res);)
			else if t < k then 
				queue := PrioQueue.insert !queue (new_d, res); 
			insertion s2 
	in
	insertion seq;
	print_string "		Insertion ok\n";
	PrioQueue.to_unsorted_list !queue

	

let classify seq k data = (*Faire un tas max de taille au plus k*)
	let l = k_closest seq k data in
	most_frequent l


let mid l = 
	let t = List.length l in
	let r = ref l in 
	for i = 0 to (t-1)/2 do 
		r := List.tl !r 
	done;
	List.hd !r


let mediane l = 
	mid (List.sort Stdlib.compare l)


let dimensional_tree seq =
	let l = List.of_seq seq in 
	let n = Array.length (fst (List.hd l)) in

	let rec insert a1 a2 t i md = 
		match t with 
		|  [] -> ()
		|  (im, res) :: t' -> 
			(if im.(i mod n) < md then 
				a1 := (im, res) :: !a1
			else if im.(i mod n) > md then 
				a2 := (im, res) :: !a2);
			insert a1 a2 t' i md
	in

	let rec step_i l i = (*Construit l'arbre n-dimensionnel et renvoie le nbr de noeuds*)
		match l with 
		|  [] -> failwith "N'arrive pas" 
		|  [x, n] -> Nil (x, n), 1
		|  _ -> 
			let l1 = List.map (fun (im, res) -> im.(i mod n)) l in 
			let md = mediane l1 in
			let a1 = ref [] in 
			let a2 = ref [] in 
			insert a1 a2 l i md; 
			let arbre_g, ng= step_i !a1 (i+1) in 
			let arbre_d, nd = step_i !a2 (i+1) in 
			Noeud(arbre_g, md, ng + nd, arbre_d), ng + nd

	in 

	step_i l 0


let rec all_smaller l data dist = (*dist est la distance de data a la mediane*)
	match l with 
	|  [] -> true 
	|  (im, res) :: t -> 
		let d = euclidean_dist im data in 
		d <= float_of_int dist && all_smaller t data dist
	



let k_closest_modified seq k data = (*Renvoie les éléments les plus proches*)
	let queue = ref (PrioQueue.create (fun (_, _, a) (_, _, b) -> Float.compare b a)) in
	let rec insertion s =
		match s with 
		|  [] -> ()
		|  (im, res) :: s2 -> 
			let new_d = euclidean_dist im data in
			let t = PrioQueue.size !queue in
			let _, _, dt = PrioQueue.top !queue in
			if t = k && new_d < dt then (
				queue := PrioQueue.remove_top !queue;
				queue := PrioQueue.insert !queue (im, res, new_d);)
			else if t < k then 
				queue := PrioQueue.insert !queue (im, res, new_d); 
			insertion s2 
	in
	insertion seq;
	print_string "		Insertion ok\n";
	let l = PrioQueue.to_unsorted_list !queue in 
	List.map (fun (a, b, c) -> (a, b)) l, List.length l


let classify_2 arbre data = 
	let k = Array.length data in

	let traitement_both kvoisc tc kvoisn tn = 
		let vois_list = kvoisc @ kvoisn in
		if tn + tc <= k then 
			vois_list, tn + tc
		else 
			k_closest_modified vois_list k data
	in

	let rec find arb i = (*Renvoie des plus proches voisins et leur nombre*)
		match arb with 
		|  Nil (x, n) -> [(x, n)], 1

		|  Noeud(arbg, md, n, arbd) -> 
			let chosen = if data.(i mod n) < md then arbg else arbd in 
			let not_ch = if data.(i mod n) >= md then arbd else arbg in 
			let kvoisc, tc = find chosen (i+1) in 
			if tc <= k then 
				if all_smaller kvoisc data md then 
					kvoisc, tc 
				else 
					let kvoisn, tn = find not_ch (i+1) in  
					traitement_both kvoisc tc kvoisn tn
			else failwith "Too much"
	in

	let k_vois, _ = find arbre 0 in 
	most_frequent k_vois



