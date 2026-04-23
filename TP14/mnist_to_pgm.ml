let output_image outfile image =
	let size = 28 in
	Printf.fprintf outfile "P2\n%d %d\n255\n" size size;
	for i = 0 to size - 1 do
		for j = 0 to size - 1 do
			Printf.fprintf outfile "%d " image.(i * size + j);
		done;
		Printf.fprintf outfile "\n"
	done

let main () =
	let train_images = Mnist.open_in "fashion/train-images-idx3-ubyte" in
	let train_labels = Mnist.open_in "fashion/train-labels-idx1-ubyte" in
	let index = int_of_string Sys.argv.(1) in
	let image = Mnist.get train_images index in
	let number = (Mnist.get train_labels index).(0) in
	let outfile = open_out ("mnist_" ^ (string_of_int index) ^ ".pgm") in
	output_image outfile image;
	close_out outfile;
	Printf.printf "Label: %d\n" number

let _ = main ()
