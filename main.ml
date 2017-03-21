open Expr
open Print_formula
open Dict_list
open Dict_litHash
open Build_ROBDD
open Tseitin
open Minisat

open Sifting
open Sifting_utils
open Robdd_to_tree

module OBDD_Build = ROBDD_BUILDER(ROBDD_LITHASH) (* change here to select the dictionary implementation *)

(* names of files used by default to print outputs *)
let robddDot = "out/ROBDD"
let propDot = "out/Formula"
let fileDot name = name^".dot"
let cnf_file = "out/test.cnf"

(* some global references and function to manage command line arguments*)

(* change the input : stdin by default *)
let input_formula = ref stdin
let set_input file =
  let channel = open_in file in
  input_formula := channel;;

let tseitin_enable = ref false
let minisat_enable = ref false
let active_minisat () =
  tseitin_enable := true;
  minisat_enable := true;;
let sift_enable = ref true

let options_list = [
  ("-tseitin", Arg.Set (tseitin_enable), "translate to SAT format");
  ("-minisat", Arg.Unit (active_minisat), "translate to SAT format and solve it with minisat");
  ("-nosift", Arg.Clear (sift_enable), "disable sifting")];;

let usage_msg = "Please read rapport.pdf instead of trying weird things. The followings options are enabled :"

(* manage tseitin transformation and interaction with minisat according to the options of command line*)
let cnf_utilities formula tree =
  if !tseitin_enable then
    begin
      let cnf = to_cnf formula in
      printCNF cnf cnf_file;
      if !minisat_enable
      then comp_cnf_robdd cnf_file formula tree;
    end
;;

(* manage ROBDD and sifting according to the options of command line*)
let sift_utilities formula =
  let sift = make_robdd_sifting formula in
  Printf.printf "Size before sifting : %d\n" sift.size;
  sifting sift;
  Printf.printf "Size after sifting : %d\n" sift.size;
  sift_to_robdd sift;;

(* logical core of the program *)
let process formula =
  begin
    try
      printPropFormula formula; (* print the formula to help debug *)
      prop_to_dot formula (fileDot propDot); (* print expression tree to help the debug *)
      let tree, nodes = if !sift_enable then sift_utilities formula else OBDD_Build.create formula in (* use or don't use the sifting, that is the question *)
      cnf_utilities formula tree;
      tree_to_dot nodes (fileDot robddDot);
      print_newline();
    with
    | Failure(s) -> print_string "Error : "; print_string s; print_newline ();
    | Not_found -> print_string "Not found\n";
    | s -> print_string "Unknown error\n"; raise s
  end

let compute () =
  Arg.parse options_list set_input usage_msg; (
    try
      let lexbuf = Lexing.from_channel (!input_formula) in
      let parse () = Parser.main Lexer.token lexbuf in (* use the parser to read input *)
      let result = parse () in
      process result; flush stdout
    with
    | s -> print_string "Typo error\n"; raise s);
  if !input_formula != stdin then close_in (!input_formula);; (* don't forget to close the channel *)

let _ = compute ()
