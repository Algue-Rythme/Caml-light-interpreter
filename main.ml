open Expr
open Print_formula

open Dict_litHash
open Dict_list
open Build_ROBDD

open Tseitin
open Sifting

open Tests

module OBDD_Build = ROBDD_BUILDER(ROBDD_LIST) (* change here to select the dictionary implementation *)

let robddDot = "/tmp/ROBDD"
let propDot = "/tmp/Formula"
let fileDot name = String.concat "" [name; ".dot"]

let compile f =
  begin
    try
      printPropFormula f;
      printCNF (to_cnf f) "sat.txt";
      prop_to_dot f (fileDot propDot);
      let tree, nodes = OBDD_Build.create f in
      tree_to_dot nodes (fileDot robddDot);
      print_newline();
    with
    | Failure(s) -> print_string "Error : "; print_string s; print_string "\n";
    | _ -> print_string "Unknow error"
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
