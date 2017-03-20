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

module OBDD_Build = ROBDD_BUILDER(ROBDD_LIST) (* change here to select the dictionary implementation *)

let robddDot = "out/ROBDD"
let propDot = "out/Formula"
let fileDot name = name^".dot"
let cnf_file = "out/test.cnf"

let input_formula = ref stdin
let tseitin_enable = ref false
let minisat_enable = ref false
let formula_input = ref stdin
let active_minisat () =
  tseitin_enable := true;
  minisat_enable := true;;

let options_list = [
  ("-tseitin", Arg.Set (tseitin_enable), "translate to SAT format");
  ("-minisat", Arg.Unit (active_minisat), "translate to SAT format and solve it with minisat")];;

let usage_msg = "Please read rapport.pdf instead of trying weird things. The followings options are enabled :"

let cnf_utilities formula tree =
  if !tseitin_enable then
    begin
      let cnf = to_cnf formula in
      printCNF cnf cnf_file;
      if !minisat_enable
      then comp_cnf_robdd cnf_file formula tree;
    end
;;

let process f =
  begin
    try
      printPropFormula f;
      prop_to_dot f (fileDot propDot);
      let sift = make_robdd_sifting f in
      let tree, nodes = sift_to_robdd sift in
      cnf_utilities f tree;
      tree_to_dot nodes (fileDot robddDot);
      print_newline();
    with
    | Failure(s) -> print_string "Error : "; print_string s; print_newline ();
    | Not_found -> print_string "Not found\n";
    | s -> print_string "Unknown error\n"; raise s
  end

let compute () =
  Arg.parse options_list print_endline usage_msg; (
  try
    let lexbuf = Lexing.from_channel (!formula_input) in
    let parse () = Parser.main Lexer.token lexbuf in
    let result = parse () in
    process result; flush stdout
  with
  | s -> print_string "Typo error\n"; raise s);
  if !input_formula != stdin then close_in (!formula_input);;

let _ = compute ()
