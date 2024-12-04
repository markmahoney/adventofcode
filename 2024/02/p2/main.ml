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

(* per line, we test each value against the previous one for distance and consistency of incremental direction *)
let distance x y = let d = Int.abs (x - y) in (d > 0 && d < 4)
let increasing x y = x < y
let decreasing x y = x > y
let is_safe_in_direction direction x y = distance x y && direction x y

(* remove only the first element from the list that fails the predicate, if any *)
let rec dampen f a =
  let remove_one x y t = match t with
    | z :: u -> if f x z then x :: t else y :: t
    | [] -> x :: t
  in
  match a with
  | x :: y :: t -> if f x y then x :: dampen f (y :: t) else remove_one x y t
  | h :: [] -> h :: []
  | [] -> []

let rec test_list f a =
  match a with
  | x :: y :: t -> if f x y then test_list f (y :: t) else false
  | _ -> true
  
let is_safe_increasing levels =
  let test = is_safe_in_direction increasing in
  test_list test (dampen test levels)

let is_safe_decreasing levels =
  let test = is_safe_in_direction decreasing in
  test_list test (dampen test levels)

let is_safe levels = (is_safe_increasing levels) || (is_safe_decreasing levels)

let () = List.iteri (fun i x -> Printf.printf "%d. %b\n" (i + 1) x) (List.map is_safe all_levels)
    
let safe_count = List.map is_safe all_levels
                 |> List.filter Fun.id
                 |> List.length
                                                    
(* print the answer *)
let () = print_endline (string_of_int safe_count)
