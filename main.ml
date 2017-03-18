open Expr
open Print_formula

open Dict_litHash
open Dict_list
open Dict_litHash
open Build_ROBDD

open Tseitin
open Sifting_utils
open Robdd_to_tree
open Sifting

open Tests

module OBDD_Build = ROBDD_BUILDER(ROBDD_LITHASH) (* change here to select the dictionary implementation *)

let robddDot = "./ROBDD"
let propDot = "./Formula"
let fileDot name = String.concat "" [name; ".dot"]

let compile f =
  begin
    try
      printPropFormula f;
      (*printCNF (to_cnf f) "sat.txt";*)
      (*prop_to_dot f (fileDot propDot);*)
      let sift = make_robdd_sifting f in
      let robdd, nodes = sift_to_robdd sift in
      (*let robdd, nodes = node_list_memory sift in*)
      let robdd_classique, nodes_classique = OBDD_Build.create f in
      (*tree_to_dot nodes_classique (fileDot robddDot);*)
      tree_to_dot nodes (fileDot robddDot) "Graphe";
      print_newline();
    with
    | Failure(s) -> print_string "Error : "; print_string s; print_string "\n";
    | Not_found -> print_string "Error : not found\n"
    | _ -> print_string "Unknown error"
  end

let calc () =
  let channel = stdin in
  try
    let lexbuf = Lexing.from_channel channel in
    let parse () = Parser.main Lexer.token lexbuf in
    let result = parse () in
    compile result; flush stdout
  with
  | _ -> (print_string "typo error\n")
;;

let _ = calc ()
