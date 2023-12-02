let day_1_data_lines = Utils.read_lines "data/day1/part1.txt"

let part_1_solution =
  let first_digit line = Utils.char_to_int (List.find Utils.is_digit (Utils.to_chars line)) in
  let last_digit line = Utils.char_to_int (List.find Utils.is_digit (List.rev (Utils.to_chars line))) in
  let first_and_last_digit line = ((first_digit line), (last_digit line)) in
  let first_and_last_number_of_each_line = List.map first_and_last_digit day_1_data_lines in
  let combined_first_and_last_digits_of_each_line = List.map (fun (a, b) -> a * 10 + b) first_and_last_number_of_each_line in
  Utils.sum combined_first_and_last_digits_of_each_line

let part_2_solution = "TODO: unimplemented";;

let run = function
  | Utils.One -> print_endline (string_of_int part_1_solution)
  | Utils.Two -> print_endline part_2_solution
