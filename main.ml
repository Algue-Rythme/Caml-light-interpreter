open Expr;;
open Dict_list;;
open Build_ROBDD;;
open Print_formula;;
open Tseitin;;

module OBDD_List = ROBDD_BUILDER(ROBDD_LIST);; (* change here to select the dictionary implementation *)

let robddDot = "/tmp/ROBDD"
let propDot = "/tmp/Formula"
let fileDot name = String.concat "" [name; ".dot"]

let compile f =
  begin
    printPropFormula f;
    printCNF (to_cnf f) "sat.txt";
    prop_to_dot f (fileDot propDot);
    let tree, nodes = OBDD_List.create f in
    tree_to_dot nodes (fileDot robddDot);
    print_newline();
  end

let lexbuf = Lexing.from_channel stdin
let parse () = Parser.main Lexer.token lexbuf

let calc () =
  try
    let result = parse () in
    compile result; flush stdout
  with _ -> (print_string "erreur de saisie\n")
;;

let _ = calc()
