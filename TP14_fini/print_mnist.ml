let print_image image =
	let gray_in = open_in "grayscale_10_levels.txt" in
	let grayscale = input_line gray_in in
	let size = 28 in
	for i = 0 to size - 1 do
		for j = 0 to size - 1 do
                        Printf.printf "%c" grayscale.[(255 - image.(i * size + j)) * (String.length grayscale - 1) / 255]
		done;
		Printf.printf "\n"
	done

let main () =
	let train_images = Mnist.open_in "fashion/train-images-idx3-ubyte" in
	let train_labels = Mnist.open_in "fashion/train-labels-idx1-ubyte" in
	let index = int_of_string Sys.argv.(1) in
	let image = Mnist.get train_images index in
	let number = (Mnist.get train_labels index).(0) in
	print_image image;
	Printf.printf "Label: %d\n" number

let _ = main ()
