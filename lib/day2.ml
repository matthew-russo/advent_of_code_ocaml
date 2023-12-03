let day_2_data_lines = Utils.read_lines "data/day2.txt"

type dice = { red: int; green: int; blue: int; }
type game = { id: int; dice: dice; }

(* val parse_line : string -> game *)

module StringMap = Map.Make (String)

let parse_id s = match String.split_on_char ' ' s with
  | game_str::id::[] -> if game_str <> "Game" then (
    Printf.printf "'%s'\n%!" game_str;
    raise (Utils.InvalidState "First piece of game specifier wasn't `Game`")
  )
  else
    int_of_string id
  | _ -> raise (Utils.InvalidState "Multiple ` ` in game specifier")

let get_or_default key map = (Option.value (StringMap.find_opt key map) ~default:0)

let accumulate_dice acc dice =
  let color_name = List.nth dice 1 in
  let color_count = int_of_string (List.nth dice 0) in
  StringMap.add color_name (max color_count (get_or_default color_name acc)) acc

let parse_set acc set =
  let dice_defs = String.split_on_char ',' set in
  let dice_defs = List.map (fun s -> String.split_on_char ' ' (String.trim s)) dice_defs in
  List.fold_left accumulate_dice acc dice_defs

let parse_dice s =
  let sets = String.split_on_char ';' s in
  let dice = List.fold_left parse_set StringMap.empty sets in
  { red = get_or_default "red" dice; green = get_or_default "green" dice; blue = get_or_default "blue" dice; };;

let parse_line s = match String.split_on_char ':' s with
  | game::dice::[] -> { id = parse_id game; dice = parse_dice dice }
  | _ -> raise (Utils.InvalidState "Multiple `:` in line")

let game_is_valid game = game.dice.red <= 12 && game.dice.green <= 13 && game.dice.blue <= 14

let part_1_solution =
  let games = List.map parse_line day_2_data_lines in
  let valid_games = List.filter game_is_valid games in
  let valid_game_ids = List.map (fun g -> g.id) valid_games in
  Utils.sum valid_game_ids
let part_2_solution = "TODO: unimplemented"

let run = function
  | Utils.One -> print_endline (string_of_int part_1_solution)
  | Utils.Two -> print_endline part_2_solution
