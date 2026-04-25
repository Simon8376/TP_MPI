(* On prépare une fonction main. En C, on est obligé d'écrire une fonction qui
   porte ce nom et c'est par elle que débute l'exécution. Ce n'est pas le cas
   en OCaml, qui exécute systématiquement tout code qui se trouve en dehors
   d'une fonction. Mais la convention du C est relativement propre, donc on la
   reprend ici. On se chargera d'exécuter manuellement [main] à la fin du
   fichier. *)

let main () =
	let train_images = Mnist.open_in "fashion/train-images-idx3-ubyte" in
	let train_labels = Mnist.open_in "fashion/train-labels-idx1-ubyte" in
	let test_images = Mnist.open_in "fashion/t10k-images-idx3-ubyte" in
	let test_labels = Mnist.open_in "fashion/t10k-labels-idx1-ubyte" in
   
   let train = Mnist.mnist_list train_images train_labels in
   let k = 3 in
   let i = ref 0 in
   try
      while !i < 50 do 
         let data = Mnist.get test_images !i in 
         let res = Mnist.get test_labels !i in
         Printf.printf "Test %d: Found %d where should have %d\n" !i (Knn.classify_2 train k data) res.(0);
         incr i
      done
   with _ -> Printf.printf "Failed after %i tests\n" !i

(* On exécute la fonction main définie précédemment *)
let _ = main ()
