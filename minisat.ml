open Scanf;;
open Printf;;
open Dictionary;;
open Tseitin;;
open Print_formula;;

let sat_file = "out/sat.txt"

let call_minisat cnf_file sat_file =
  let _ = Sys.command ("minisat "^cnf_file^" "^sat_file) in ();;

exception Unsat;;
let check_minisat sat_file =
  let channel = open_in sat_file in
  let ib = Scanning.from_channel channel in
  match bscanf ib "%s\n" (fun v -> v) with
  | answer when answer = "UNSAT" -> close_in channel; Array.make 0 false
  | _ ->
    let rec read_var () =
      let v = bscanf ib "%d%c" (fun v c -> v) in
      if v == 0 then (print_newline (); [])
      else (
        printf "%d = %s\n" (abs v) (if v > 0 then "True" else "False");
        (abs v, v > 0)::(read_var ())
      )
    in
    let vars = read_var () in
    close_in channel;
    let valuation = Array.make ((List.length vars)+1) false in
    List.iter (function (i, v) -> valuation.(i) <- v) vars;
    valuation;;

exception ROBDD_CNF_Incompatibility;;
let check_robdd valuation robdd =
  let rec visit = function
    | LeafFalse -> raise ROBDD_CNF_Incompatibility
    | LeafTrue -> ()
    | Node(Var(i), l, r) ->
      let v = if i > 0 then valuation.(abs i) else not valuation.(abs i) in
      if v then visit r else visit l
  in visit robdd;;

let comp_cnf_robdd cnf_file formula robdd =
  call_minisat cnf_file sat_file;
  try
    let valuation = check_minisat sat_file in
    ( match robdd with
    | LeafFalse when (Array.length valuation) <> 0 -> raise ROBDD_CNF_Incompatibility
    | LeafFalse -> ()
    | _ -> check_robdd valuation robdd
    );
      print_string "ROBDD and CNF match !\n"
  with
  | ROBDD_CNF_Incompatibility ->
    print_string "ROBDD and CNF are not compatibles\nCongratulation, you just found a bug !\n"
  | s -> raise s
;;
