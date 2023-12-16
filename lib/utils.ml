type part = One | Two | Debug

exception InvalidState of string

(* given a filename, read the entire file to a string *)
let read_file file = In_channel.with_open_bin file In_channel.input_all

(* given a filename, read it to a list of strings, one for each line *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' (String.trim contents)

(* given a string, return a list of its characters *)
let to_chars input = input |> String.to_seq |> List.of_seq

(* a function that will validate if the input is a digit *)
let is_digit digit = match digit with '0' .. '9' -> true | _ -> false

(* a function that will add the elements of a list together *)
let sum xs = List.fold_left (fun x y -> x + y) 0 xs

(* convert a char to its integer repr, e.g. '1' -> 1, '7' -> 7 *)
let char_to_int c = int_of_string (String.make 1 c)

(* generate a range of `start`..(`start` + `len) *)
let rec range ?(start=0) len =
  if start >= len
  then []
  else start :: (range len ~start:(start+1))

let lazy_range ?(from=1) ?(step=1) until =
  let cmp = match step with
    | i when i < 0 -> (>)
    | i when i > 0 -> (<)
    | _ -> raise (Invalid_argument "step cannot be 0")
  in
  Seq.unfold (function
        | i when cmp i until -> Some (i, i + step)
        | _ -> None
    ) from

(* filter a list of strings, removing empty ones *)
let filter_empty s = List.filter (fun s -> s <> "") s

(* split `xs` in to chunks of `n` size *)
let rec chunks n xs =
  let rec take k xs ys = match k, xs with
    | 0, _ -> List.rev ys :: chunks n xs
    | _, [] -> if ys = [] then [] else [ys]
    | _, x::xs' -> take (k - 1) xs' (x::ys)
  in take n xs []