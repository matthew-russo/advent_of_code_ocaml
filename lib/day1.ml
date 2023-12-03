let day_1_data_lines = Utils.read_lines "data/day1.txt"

let combine_first_and_last_digits digits = List.map (fun (a, b) -> a * 10 + b) digits

let part_1_solution =
  let first_digit line = Utils.char_to_int (List.find Utils.is_digit (Utils.to_chars line)) in
  let last_digit line = Utils.char_to_int (List.find Utils.is_digit (List.rev (Utils.to_chars line))) in
  let first_and_last_digit line = ((first_digit line), (last_digit line)) in
  let first_and_last_number_of_each_line = List.map first_and_last_digit day_1_data_lines in
  Utils.sum (combine_first_and_last_digits first_and_last_number_of_each_line)

type state = {first : int option; last : int option;}

let empty_state = {first = None; last = None;}

let chomp_next s =
  if String.starts_with ~prefix:"one" s then (Some 1, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"two" s then (Some 2, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"three" s then (Some 3, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"four" s then (Some 4, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"five" s then (Some 5, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"six" s then (Some 6, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"seven" s then (Some 7, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"eight" s then (Some 8, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"nine" s then (Some 9, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"1" s then (Some 1, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"2" s then (Some 2, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"3" s then (Some 3, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"4" s then (Some 4, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"5" s then (Some 5, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"6" s then (Some 6, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"7" s then (Some 7, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"8" s then (Some 8, String.sub s 1 ((String.length s) - 1))
  else if String.starts_with ~prefix:"9" s then (Some 9, String.sub s 1 ((String.length s) - 1))
  else (None, String.sub s 1 ((String.length s) - 1))

let consume_token current_state chomped = match (current_state, chomped) with
    ({first = None; last = Some _}, _) -> raise (Utils.InvalidState "first is None but last is Some")
  | ({first = None; last = None}, Some c) -> {first = Some c; last = Some c}
  | ({first = Some f; _}, Some c) -> {first = Some f; last = Some c}
  | (s, None) -> s
;;

let rec parse_line current_state line =
  if String.empty = line then current_state
  else begin
    let (token, rest) = chomp_next line in
    parse_line (consume_token current_state token) rest
  end

let unwrap_state = function
  | { first = Some a; last = Some b } -> (a, b)
  | _ -> raise (Utils.InvalidState "first and last are not some")

let part_2_solution =
  let parsed_line_states = List.map (fun l -> parse_line empty_state l) day_1_data_lines in
  let first_and_last_number_of_each_line = List.map unwrap_state parsed_line_states in
  Utils.sum (combine_first_and_last_digits first_and_last_number_of_each_line)

let run = function
  | Utils.One -> print_endline (string_of_int part_1_solution)
  | Utils.Two -> print_endline (string_of_int part_2_solution)
