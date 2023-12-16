let usage_msg = "advent-of-code <day> <part>"
let day = ref 0
let part = ref 0

let speclist =
  [ ("-day", Arg.Set_int day, "day"); ("-part", Arg.Set_int part, "part") ]

let () = Arg.parse speclist (fun _ -> ()) usage_msg;;

if !day = 0 then failwith "day is a required arg";;
if !part = 0 then failwith "part is a required arg";;

Printf.printf "Running advent of code solution for Day %d, Part %d\n%!",
  !day,
  !part

let run = function
  | 1, 3 -> AdventOfCode.Day1.run Debug
  | 1, 1 -> AdventOfCode.Day1.run One
  | 1, 2 -> AdventOfCode.Day1.run Two
  | 2, 3 -> AdventOfCode.Day2.run Debug
  | 2, 1 -> AdventOfCode.Day2.run One
  | 2, 2 -> AdventOfCode.Day2.run Two
  | 3, 3 -> AdventOfCode.Day3.run Debug
  | 3, 1 -> AdventOfCode.Day3.run One
  | 3, 2 -> AdventOfCode.Day3.run Two
  | 4, 3 -> AdventOfCode.Day4.run Debug
  | 4, 1 -> AdventOfCode.Day4.run One
  | 4, 2 -> AdventOfCode.Day4.run Two
  | 5, 3 -> AdventOfCode.Day5.run Debug
  | 5, 1 -> AdventOfCode.Day5.run One
  | 5, 2 -> AdventOfCode.Day5.run Two
  | 6, 3 -> AdventOfCode.Day6.run Debug
  | 6, 1 -> AdventOfCode.Day6.run One
  | 6, 2 -> AdventOfCode.Day6.run Two
  | _ -> raise (Invalid_argument "unknown day and part")
;;

run (!day, !part)
