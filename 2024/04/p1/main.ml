(* get and parse the input from stdin *)
let rec read_lines ic =
  try
    let line = input_line ic in
    line :: read_lines ic
  with End_of_file ->
    []

let grid = read_lines stdin
let flat_grid = List.fold_left (fun acc s -> acc ^ s) "" grid

(* we're taking it on faith that all the rows are the same length, because otherwise ??? *)
let cols = String.length (List.hd grid)
let rows = List.length grid

(* the offsets we'll use to check all the strings surrounding any X we find *)
let offsets = [
  -cols - 1; -cols;  -cols + 1;
  -1;       (*0;*)   1;
  cols - 1;  cols;   cols + 1
]

let fits pos off =
  let farthest = pos + off * 3 in
  let p_col = pos mod cols in
  let f_row = farthest / cols in
  let f_col = farthest mod cols in
  (* make sure we don't fall out of the bounds of the grid itself *)
  f_row >= 0 && f_row < rows && f_col >= 0 && f_col < cols &&
  (* make sure we also didn't wrap around; this returns false positive for very small grids but whatever *)
  Int.abs (p_col - f_col) < 4

let is_xmas pos off =
  (fits pos off) &&
  (flat_grid.[pos] = 'X') &&
  (flat_grid.[pos + off] = 'M') &&
  (flat_grid.[pos + off * 2] = 'A') &&
  (flat_grid.[pos + off * 3] = 'S')

let rec count start =
  try
    let pos = String.index_from flat_grid start 'X' in
    let acc = List.fold_left (fun acc off -> acc + if (is_xmas pos off) then 1 else 0) 0 offsets in
    acc + count (pos + 1)
  with Not_found -> 0

let () = print_endline (string_of_int (count 0))
