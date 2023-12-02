let usage_msg = "advent-of-code <day> <part>"

let day = ref 0
let part = ref 0

let speclist =
  [
    ("-day", Arg.Set_int day, "day");
    ("-part", Arg.Set_int part, "part");
  ]

let () = Arg.parse speclist (fun _ -> ()) usage_msg;;

if !day = 0 then failwith "day is a required arg";;
if !part = 0 then failwith "part is a required arg";;

Printf.printf "Running advent of code solution for Day %d, Part %d\n%!", !day, !part;;

let run = function
  | (1, 1) -> AdventOfCode.Day1.run One
  | (1, 2) -> AdventOfCode.Day1.run Two
  | _ -> raise (Invalid_argument "unknown day and part");;

run (!day, !part)
