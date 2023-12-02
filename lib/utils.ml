type part = One | Two

(* given a filename, read the entire file to a string *)
let read_file file =
  In_channel.with_open_bin file In_channel.input_all

(* given a filename, read it to a list of strings, one for each line *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' (String.trim contents)

(* given a string, return a list of its characters *)
let to_chars input = input |> String.to_seq |> List.of_seq

(* a function that will validate if the input is a digit *)
let is_digit digit =
    match digit with
     '0' .. '9' -> true
    | _ -> false

(* a function that will add the elements of a list together *)
let sum xs = List.fold_left (fun x y -> x + y) 0 xs

(* convert a char to its integer repr, e.g. '1' -> 1, '7' -> 7 *)
let char_to_int c = int_of_string (String.make 1 c)
