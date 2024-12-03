(* get and parse the input from stdin *)
let rec read_lines ic =
  try
    let line = input_line ic in
    line :: read_lines ic
  with End_of_file ->
    []

let level_lines = read_lines stdin

let whitespace = Str.regexp {|[ \t]+|}
let process_line line = Str.split whitespace line
                        |> List.filter((<>) "")
                        |> List.map int_of_string
let all_levels = List.map process_line level_lines

(* turn each list of level values into a list of tuples, pairing each entry with its neighbor to the right *)
let pair_levels levels =
  (* get every element except the last *)
  let a = List.rev levels
          |> List.tl
          |> List.rev in
  (* get every element except the first *)
  let b = List.tl levels in
  (* pair them *)
  List.combine a b

let all_level_pairs = List.map pair_levels all_levels

(* now, per line of pairs, we test each pair for distance and consistency of incremental direction *)
let is_safe_distance x y = let distance = Int.abs (x - y) in (distance > 0 && distance < 4)

let increasing x y = x > y
let decreasing x y = x < y
let is_safe_in_direction direction prev (x, y) = prev && is_safe_distance x y && direction x y
let is_safe_increasing level_pairs = List.fold_left (is_safe_in_direction increasing) true level_pairs
let is_safe_decreasing level_pairs = List.fold_left (is_safe_in_direction decreasing) true level_pairs

let is_safe level_pairs = is_safe_increasing level_pairs || is_safe_decreasing level_pairs

let safe_count = List.map is_safe all_level_pairs
                 |> List.filter (fun a -> a)
                 |> List.length
                                                    
(* print the answer *)
let () = print_endline (string_of_int safe_count)
