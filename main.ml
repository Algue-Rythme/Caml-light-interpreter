open Expr
open OBDD_list
  
let compile e =
  begin
    printPropFormula e;
    print_newline();
    (* print_int (evalPropFormula e); *)
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
