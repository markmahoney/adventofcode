(* get the lists from stdin *)
let rec read_lines ic =
  try
    let line = input_line ic in
    line :: read_lines ic
  with End_of_file ->
    []

let ids = read_lines stdin

(* split the lists *)
let split_pair id_line =
  let r = Str.regexp {|[ \t]+|} in
  let t = Str.split r id_line |> List.filter((<>) "") in
  (int_of_string (List.hd t), int_of_string (List.rev t |> List.hd))

let a, b = List.map split_pair ids |> List.split

(* sort the lists and combine them *)
let sorted_pairs = List.combine (List.sort compare a) (List.sort compare b)
let distances = List.map (fun (a, b) -> Int.abs(a - b)) sorted_pairs
let total_distance = List.fold_left ( + ) 0 distances

(* print the answer *)
let () = print_endline (string_of_int total_distance)

