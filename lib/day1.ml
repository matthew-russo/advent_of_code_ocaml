let first_number line = Utils.char_to_int (List.find Utils.is_digit (Utils.to_chars line))

let last_number line = Utils.char_to_int (List.find Utils.is_digit (List.rev (Utils.to_chars line)))

let first_and_last_number line = ((first_number line), (last_number line))

let day_1_data_lines = Utils.read_lines "data/day1/part1.txt"

let first_and_last_number_of_each_line = List.map first_and_last_number day_1_data_lines

let combined_first_and_last_numbers_of_each_line = List.map (fun (a, b) -> a * 10 + b) first_and_last_number_of_each_line

let solution = Utils.sum combined_first_and_last_numbers_of_each_line

let run = print_endline (string_of_int solution)
