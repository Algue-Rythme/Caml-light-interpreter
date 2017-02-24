open Expr;;
open Dict_list;;
open BuildROBDD;;

module OBDD_List = ROBDD_BUILDER(ROBDD_LIST);;

let name = "/tmp/out"
let fileDot = String.concat "" [name; ".dot"]

let compile f =
  begin
    printPropFormula f;
    let tree, nodes = OBDD_List.create f in
    to_dot nodes fileDot;
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
