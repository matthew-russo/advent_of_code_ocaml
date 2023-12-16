let day_5_data_lines = Utils.read_lines "data/day5.txt"

type range = {
  src : int;
  dst : int;
  size : int;
}
let range_contains range n = range.src <= n && (range.src + range.size) > n

type resource_map = {
  ranges : range list;
}
let empty_resource_map = {
  ranges = [];
}
let add_range map range = {
  ranges = range :: map.ranges;
}
let lookup map n =
  match List.find_opt (fun r -> range_contains r n) map.ranges with
  | Some r -> n - r.src + r.dst
  | None -> n

type map_ty =
  | Control
  | SeedToSoil
  | SoilToFertilizer
  | FertilizerToWater
  | WaterToLight
  | LightToTemp
  | TempToHumidity
  | HumidityToLocation
let string_of_map_ty ty =
  match ty with
  | Control -> "control"
  | SeedToSoil -> "seed-to-soil"
  | SoilToFertilizer -> "soil-to-fertilizer"
  | FertilizerToWater -> "fertilizer-to-water"
  | WaterToLight -> "water-to-light"
  | LightToTemp -> "light-to-temp"
  | TempToHumidity -> "temp-to-humidity"
  | HumidityToLocation -> "humidity-to-location"

module AlmanacResourceMap = Map.Make (
  struct
    let compare = compare
    type t = map_ty
  end
)

type almanac = {
  seeds : int list;
  resources : resource_map AlmanacResourceMap.t;
}

type parse_state = {
  seeds : int list;
  resources : resource_map AlmanacResourceMap.t;
  curr_map : resource_map;
  curr_ty : map_ty;
}
let empty_parse_state = {
  seeds = [];
  resources = AlmanacResourceMap.empty;
  curr_map = empty_resource_map;
  curr_ty = Control;
}
let with_seeds state seeds = {
  seeds = seeds;
  resources = state.resources;
  curr_map = state.curr_map;
  curr_ty = state.curr_ty;
}
let with_ty state map_ty = {
  seeds = state.seeds;
  resources = state.resources;
  curr_map = state.curr_map;
  curr_ty = map_ty;
}
let with_range state range = {
  seeds = state.seeds;
  resources = state.resources;
  curr_map = add_range state.curr_map range;
  curr_ty = state.curr_ty;
}
let flush state =
  match state.curr_ty with
    (* if we're in the Control state then do nothing, aren't parsing anything yet *)
    Control -> state
  | _ -> {
    seeds = state.seeds;
    resources = AlmanacResourceMap.add state.curr_ty state.curr_map state.resources;
    curr_map = empty_resource_map;
    curr_ty = Control;
  }

(* parsing *)

let parse_seeds state line =
  match String.split_on_char ':' line with
  | [ seeds_str; seeds ] ->
      if seeds_str <> "seeds" then (
        Printf.printf "'%s'\n%!" seeds_str;
        raise (Utils.InvalidState "First piece of seeds wasn't `seeds`"))
      else
        let seeds = List.map int_of_string (Utils.filter_empty (String.split_on_char ' ' seeds)) in
        with_seeds state seeds
  | _ -> raise (Utils.InvalidState "Multiple `:` in seeds list")

let parse_map_name state name =
  match name with
  | "seed-to-soil" -> with_ty state SeedToSoil
  | "soil-to-fertilizer" -> with_ty state SoilToFertilizer
  | "fertilizer-to-water" -> with_ty state FertilizerToWater
  | "water-to-light" -> with_ty state WaterToLight
  | "light-to-temperature" -> with_ty state LightToTemp
  | "temperature-to-humidity" -> with_ty state TempToHumidity
  | "humidity-to-location" -> with_ty state HumidityToLocation
  | _ -> raise (Utils.InvalidState "Unrecognized map name")

let parse_map_decl state line =
  match String.split_on_char ' ' line with
  | [ name; map_str ] ->
      if map_str <> "map:" then (
        Printf.printf "'%s'\n%!" map_str;
        raise (Utils.InvalidState "Last piece of map wasn't `map:`"))
      else
        parse_map_name state name
  | _ -> raise (Utils.InvalidState "Multiple ` ` in map decl")

let parse_range state line =
  match String.split_on_char ' ' line with
  | [dst; src; size] -> with_range state { src = int_of_string src; dst = int_of_string dst; size = int_of_string size; }
  | _ -> raise (Utils.InvalidState "Range had more than 3 space-separated elements")

let parse_line state line =
  if String.starts_with ~prefix:"seeds:" line then
    parse_seeds state line
  else if String.ends_with ~suffix:"map:" line then
    parse_map_decl state line
  else if line = "" then
    flush state
  else
    parse_range state line

let parse_almanac lines =
  let state = List.fold_left (fun acc line -> parse_line acc line) empty_parse_state lines in
  let state = flush state in
  {
    seeds = state.seeds;
    resources = state.resources;
  }

(* solving *)

let resolve_input (al : almanac) (ty : map_ty) (input) : int =
  let map = AlmanacResourceMap.find ty al.resources in
  lookup map input

let seed_to_location (al : almanac) (seed : int) : int =
  let pipeline = [SeedToSoil; SoilToFertilizer; FertilizerToWater; WaterToLight; LightToTemp; TempToHumidity; HumidityToLocation] in
  List.fold_left (fun input map_ty -> resolve_input al map_ty input) seed pipeline

let lowest_location (al : almanac) (seeds : int list) : int =
  let locations = List.map (fun s -> seed_to_location al s) seeds in
  List.fold_left min (List.hd locations) (List.tl locations)

let part_1_solution =
  let almanac = parse_almanac day_5_data_lines in
  lowest_location almanac almanac.seeds

let part_2_solution =
  let almanac = parse_almanac day_5_data_lines in
  let seed_ranges = Utils.chunks 2 almanac.seeds in
  let seed_seqs = List.map (fun range -> Utils.lazy_range ~from:(List.hd range) (List.hd (List.tl range))) seed_ranges in
  List.fold_left (fun acc seq -> min acc (Seq.fold_left (fun acc seed -> min acc (seed_to_location almanac seed)) acc seq)) 1073741824 seed_seqs

let debug_str = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

let debug =
  let almanac = parse_almanac (String.split_on_char '\n' (String.trim debug_str)) in
  lowest_location almanac almanac.seeds

let run = function
  | Utils.One -> print_endline (string_of_int part_1_solution)
  | Utils.Two -> print_endline (string_of_int part_2_solution)
  | Utils.Debug -> print_endline (string_of_int debug)
