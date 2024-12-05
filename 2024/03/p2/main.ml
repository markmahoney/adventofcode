(* get and parse the input from stdin *)
let rec read_lines ic =
  try
    let line = input_line ic in
    line :: read_lines ic
  with End_of_file ->
    []

let input_list = read_lines stdin
    
let do_token = Str.regexp {|do()|}
let dont_token = Str.regexp {|don't()|}
let mul_token = Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|}

(* find all the mul()s in a string and return their params *)
let rec find_all_matches r s start =
  let open Str in
  try
    let _ = search_forward r s start in
    let marker = match_end () in
    let x = matched_group 1 s in
    let y = matched_group 2 s in
    (int_of_string x, int_of_string y) :: find_all_matches r s marker
  with Not_found -> []

(* take a string and map it to sections demarkated by do() and don't() tokens,
 * paired with a bool indicating whether it's a do() segment or not *)
let rec find_sections s start enabled =
  let open Str in
  try
    (* if we're in an enabled section, scan for the next disabling token, or vice versa *)
    let token = if enabled then dont_token else do_token in
    let _ = search_forward token s start in
    let marker = match_end () in
    let region = String.sub s start (marker - start) in
    (enabled, region) :: find_sections s marker (not enabled)
  with Not_found ->
    [(enabled, String.sub s start ((String.length s) - start))]

let last a = List.rev a |> List.hd

(* filter the input list down to just sections of strings that map to a do() token *)
let rec find_enabled_sections input_list enabled =
  match input_list with
  | h :: t -> 
    let section_pairs = find_sections h 0 enabled in
    (* we need to remember if we were in a do() or don't() section for the next input line *)
    let (end_state, _) = try last section_pairs with Not_found -> (enabled, "") in
    let enabled_sections = List.filter (fun (enabled, _) -> enabled) section_pairs
                           |> List.map (fun (_, sections) -> sections) in
    enabled_sections :: find_enabled_sections t end_state
  | [] -> []
    
(* collect mul() params across all enabled input line sections *)
let sections = List.flatten (find_enabled_sections input_list true)
let mul_param_list = List.flatten (List.map (fun line -> find_all_matches mul_token line 0) sections)

(* now just run the numbers *)
let final = List.fold_left (fun acc (x, y) -> x * y + acc) 0 mul_param_list

let () = print_endline (string_of_int final)
