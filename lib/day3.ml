let day_3_data_lines = Utils.read_lines "data/day3.txt"
let day_3_data_chars = List.map Utils.to_chars day_3_data_lines

type coordinate = { x : int; y : int; }
let translate coord direction = { x = coord.x + direction.x; y = coord.y + direction.y; }
let adjacent_directions = [
  { x = -1; y = 1};
  { x = -1; y = 0};
  { x = -1; y = -1};
  { x = 0; y = 1};
  { x = 0; y = -1};
  { x = 1; y = 1};
  { x = 1; y = 0};
  { x = 1; y = -1};
]

module CoordinateSet = Set.Make(struct type t = coordinate let compare = compare end)

let add_adjacent_coordinates symbol_adjacent_spots coord = List.fold_left
  (fun acc dir -> CoordinateSet.add (translate coord dir) acc )
  symbol_adjacent_spots
  adjacent_directions

type potential_part = { num : int; coordinates : coordinate list }

type schematic = { potential_parts : potential_part list; symbol_adjacent_spots : CoordinateSet.t; }
let empty_schematic = { potential_parts = []; symbol_adjacent_spots = CoordinateSet.empty; }
let add_potential_part schematic part = { potential_parts = part :: schematic.potential_parts; symbol_adjacent_spots = schematic.symbol_adjacent_spots; }
let schematic_add_symbol schematic symbol_coord = { potential_parts = schematic.potential_parts; symbol_adjacent_spots = add_adjacent_coordinates schematic.symbol_adjacent_spots symbol_coord; }

type parse_state = { schematic : schematic; row : int; col : int; pending : char list; }
let initial_parse_state = { schematic = empty_schematic; row = 0; col = 0; pending = []; }
let get_coord state = { x = state.col; y = state.row; }
let next_row state = { schematic = state.schematic; row = state.row + 1; col = 0; pending = state.pending; }
let next_col state = { schematic = state.schematic; row = state.row; col = state.col + 1; pending = state.pending; }
let prev_col state = { schematic = state.schematic; row = state.row; col = state.col - 1; pending = state.pending; }
let add_pending_character state c = { schematic = state.schematic; row = state.row; col = state.col; pending = c :: state.pending; }
let parse_state_add_symbol state symbol_coord = { schematic = schematic_add_symbol state.schematic symbol_coord; row = state.row; col = state.col; pending = state.pending; }
let flush state =
  match state.pending with
  | [] -> state
  | cs ->
    let coordinates = List.map (fun n -> { x = state.col - n - 1; y = state.row; }) (Utils.range (List.length cs)) in
    let potential_part = { num = int_of_string (String.of_seq (List.to_seq (List.rev cs))); coordinates = coordinates; } in
    { schematic = add_potential_part state.schematic potential_part; row = state.row; col = state.col; pending = []; }

let handle_char parse_state c = 
  match c with 
  | '.' -> flush parse_state
  | '0'..'9' -> add_pending_character parse_state c
  | _ -> 
    let parse_state = flush parse_state in 
    parse_state_add_symbol parse_state (get_coord parse_state)

let parse_line parse_state line =
  List.fold_left (fun acc c -> next_col (handle_char acc c)) parse_state line

let parse_schematic lines =
  List.fold_left (fun state line -> next_row (flush (prev_col (parse_line state line)))) initial_parse_state lines

let is_valid_part part spots =
  List.exists (fun coord -> Option.is_some (CoordinateSet.find_opt coord spots)) part.coordinates

let find_valid_parts schematic = 
  List.filter (fun part -> is_valid_part part schematic.symbol_adjacent_spots) schematic.potential_parts

let part_1_solution = 
  let parse_state = parse_schematic day_3_data_chars in
  let parts = find_valid_parts parse_state.schematic in
  Utils.sum (List.map (fun p -> p.num) parts)

let part_2_solution = "TODO: unimplemented"

let run = function
  | Utils.One -> print_endline (string_of_int part_1_solution)
  | Utils.Two -> print_endline part_2_solution