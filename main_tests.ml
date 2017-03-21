open Tests
open Printf

(* just emulate the parsing of command line *)

let usage_msg = "Just pass an option followed by an integer :";;

let options_list = [
  ("-parity", Arg.Int (gen_parity), "produce a formula satisfiable only with an odd number of literals set to true");
  ("-rotation", Arg.Int (gen_rotations), "something of the form 1 => 2 => 3 => ... => (n-1) => n => 1");
  ("-pigeonhole", Arg.Int (gen_pigeonhole), "print the formula of pigeonhole principle according to the example given")
];;

let _ = Arg.parse options_list print_endline usage_msg;;
