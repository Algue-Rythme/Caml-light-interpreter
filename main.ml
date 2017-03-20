open Expr
open Print_formula
open Dict_list
open Dict_litHash
open Build_ROBDD
open Minisat

open Sifting
open Sifting_utils
open Robdd_to_tree

module OBDD_Build = ROBDD_BUILDER(ROBDD_LIST) (* change here to select the dictionary implementation *)

let robddDot = "out/ROBDD"
let propDot = "out/Formula"

let fileDot name = String.concat "" [name; ".dot"]

let process f =
  begin
    try
      printPropFormula f;
      prop_to_dot f (fileDot propDot);
      (*let tree, nodes = OBDD_Build.create f in*)
      let sift = make_robdd_sifting f in
      (*let tree, nodes = node_list_memory sift in*)
      let tree, nodes = sift_to_robdd sift in
      comp_cnf_robdd "out/test.cnf" f tree;
      tree_to_dot nodes (fileDot robddDot);
      print_newline();
    with
    | Failure(s) -> print_string "Error : "; print_string s; print_newline ();
    | Not_found -> print_string "Not found\n";
    | s -> print_string "Unknown error\n"; raise s
  end

let compute () =
  let channel = if (Array.length Sys.argv) > 1 then open_in Sys.argv.(1) else stdin in
  ( try
    let lexbuf = Lexing.from_channel channel in
    let parse () = Parser.main Lexer.token lexbuf in
    let result = parse () in
    process result; flush stdout
  with
  | s -> print_string "Typo error\n"; raise s);
  close_in channel;;

let _ = compute ()
