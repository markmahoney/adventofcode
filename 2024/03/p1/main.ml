(* get and parse the input from stdin *)
let rec read_lines ic =
  try
    let line = input_line ic in
    line :: read_lines ic
  with End_of_file ->
    []

let input = read_lines stdin
    
(* scan for valid instances of mul() *)
let rec find_all_matches r s start =
  let open Str in
  try
    let _ = search_forward r s start in
    let e = match_end () in
    let x = matched_group 1 s in
    let y = matched_group 2 s in
    (int_of_string x, int_of_string y) :: find_all_matches r s e
  with Not_found -> []

let matcher = Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|}
let cmd_list = List.flatten (List.map (fun line -> find_all_matches matcher line 0 ) input)

let () = List.iter (fun (x, y) -> Printf.printf "mul(%d,%d)\n" x y) cmd_list

let final = List.fold_left (fun acc (x, y) -> x * y + acc) 0 cmd_list

let () = print_endline (string_of_int final)
