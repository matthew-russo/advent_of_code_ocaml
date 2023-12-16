let day_6_data_lines = Utils.read_lines "data/day6.txt"

type race_info = {
  times : int list;
  distances : int list;
}
let race race_info n = (List.nth race_info.times n, List.nth race_info.distances n)
let races race_info = List.combine race_info.times race_info.distances

(* parsing *)

let parse_int_list line delimiter sigil = 
  match String.split_on_char delimiter line with
  | [ sigil_str; ls ] ->
      if sigil_str <> sigil then (
        Printf.printf "'%s'\n%!" sigil_str;
        raise (Utils.InvalidState (Printf.sprintf "First piece of line wasn't `%s`" sigil)))
      else
        List.map int_of_string (Utils.filter_empty (String.split_on_char ' ' ls))
  | _ -> raise (Utils.InvalidState "Multiple delimiters in list")

let parse_race_info_part_1 lines =
  let first_line = List.hd lines in
  let times = parse_int_list first_line ':' "Time" in
  let second_line = List.hd (List.tl lines) in
  let distances = parse_int_list second_line ':' "Distance" in
  { times = times; distances = distances; }

let parse_space_separated_int line delimiter sigil =
  match String.split_on_char delimiter line with
  | [ sigil_str; ls ] ->
      if sigil_str <> sigil then (
        Printf.printf "'%s'\n%!" sigil_str;
        raise (Utils.InvalidState (Printf.sprintf "First piece of line wasn't `%s`" sigil)))
      else
        int_of_string (String.concat "" (Utils.filter_empty (String.split_on_char ' ' ls)))
  | _ -> raise (Utils.InvalidState "Multiple delimiters in list")

let parse_race_info_part_2 lines =
  let first_line = List.hd lines in
  let time = parse_space_separated_int first_line ':' "Time" in
  let second_line = List.hd (List.tl lines) in
  let distance = parse_space_separated_int second_line ':' "Distance" in
  { times = [time]; distances = [distance]; }

(* solving *)

let rec ways_to_win time_left time_held target_distance acc =
  match (time_left, time_held, target_distance) with
  | (0, _time_held, _target_distance) -> acc
  | (time_left, time_held, target_distance) ->
    let actual_distance = time_left * time_held in
    if actual_distance > target_distance then
      ways_to_win (time_left - 1) (time_held + 1) target_distance (acc + 1)
    else
      ways_to_win (time_left - 1) (time_held + 1) target_distance acc

(*
  Your toy boat has a starting speed of zero millimeters per millisecond.
  For each whole millisecond you spend at the beginning of the race holding
  down the button, the boat's speed increases by one millimeter per millisecond.
*)
let part_1_solution =
  let race_info = parse_race_info_part_1 day_6_data_lines in
  let races = races race_info in
  List.fold_left (fun acc (time, distance) -> acc * (ways_to_win time 0 distance 0)) 1 races

let part_2_solution = 
  let race_info = parse_race_info_part_2 day_6_data_lines in
  let (time, distance) = List.hd (races race_info) in
  ways_to_win time 0 distance 0

let debug = "debug"

let run = function
  | Utils.One -> print_endline (string_of_int part_1_solution)
  | Utils.Two -> print_endline (string_of_int part_2_solution)
  | Utils.Debug -> print_endline debug
