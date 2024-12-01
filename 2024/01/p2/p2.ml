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

(* count instances of each number in a that is in b, then calculate the similarity *)
let occurances x l = List.length (List.filter (fun y -> x = y) l)
let similarity = List.map (fun x -> x * occurances x b) a |> List.fold_left ( + ) 0

(* print the answer *)
let () = print_endline (string_of_int similarity)

