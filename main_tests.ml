open Tests
open Printf

let _ =
  try
    let name = Sys.argv.(1) in
    let n = int_of_string Sys.argv.(2) in
    match name with
    | "-parity" -> gen_parity n
    | "-rotations" -> gen_rotations n
    | "-pigeonhole" -> gen_pigeonhole n
    | _ -> raise (Invalid_argument("Unknown option"))
  with
  | Invalid_argument(s) ->
    begin
      printf "%s\n" s;
      printf "usage: ./generate -<test> n\n";
      printf "Available -<test> are -parity -rotations -pigeonhole\n"
    end
;;
