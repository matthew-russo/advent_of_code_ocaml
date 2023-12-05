let day_4_data_lines = Utils.read_lines "data/day4.txt"

module IntSet = Set.Make( 
  struct
    let compare = compare
    type t = int
  end
)

type card = { id : int; winning_nums : IntSet.t; our_nums : int list; }

let double n = n * 2
let split_on_space s = List.filter (fun x -> x <> String.empty) (List.map String.trim (String.split_on_char ' ' s))
let nums_of_strs ns = List.map int_of_string ns
let set_of_list l = List.fold_left (fun acc x -> IntSet.add x acc) IntSet.empty l

let parse_id s =
  match split_on_space s with
  | [ card_str; id ] ->
      if card_str <> "Card" then (
        Printf.printf "'%s'\n%!" card_str;
        raise (Utils.InvalidState "First piece of card specifier wasn't `Card`"))
      else int_of_string (String.trim id)
  | _ -> raise (Utils.InvalidState "Multiple ` ` in id")

let parse_nums s =
  match String.split_on_char '|' s with
  | [ winning; ours ] ->
    let winning_nums = set_of_list (nums_of_strs (split_on_space winning)) in
    let our_nums = nums_of_strs (split_on_space ours) in 
    (winning_nums, our_nums)
  | _ -> raise (Utils.InvalidState "Multiple `|` in nums")

let parse_line s = match String.split_on_char ':' s with
  | [ card; nums ] -> 
    let (winning, ours) = parse_nums nums in
    { id = parse_id card; winning_nums = winning; our_nums = ours; }
  | _ -> raise (Utils.InvalidState "Multiple `:` in line")

let tally card = 
  let winners = List.filter (fun n -> Option.is_some (IntSet.find_opt n card.winning_nums)) card.our_nums in
  match List.length winners with
  | 0 -> 0
  | 1 -> 1
  | _ -> List.fold_left (fun acc _ -> double acc) 1 (List.tl winners)

let part_1_solution = 
  let cards = List.map parse_line day_4_data_lines in
  let card_points = List.map tally cards in
  Utils.sum card_points

let part_2_solution = "TODO: unimplemented"

let run = function
  | Utils.One -> print_endline (string_of_int part_1_solution)
  | Utils.Two -> print_endline part_2_solution