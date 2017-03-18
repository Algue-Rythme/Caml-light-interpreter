open Scanf;;
open Dictionary;;

let call_minisat cnf_file sat_file =
  let _ = Sys.command ("minisat "^cnf_file^" "^sat_file) in ();;

exception Unsat;;
let check_minisat sat_file =
  let channel = open_in sat_file in
  match input_line channel with
  | answer when answer = "UNSAT" -> close_in channel; raise Unsat
  | _ ->
    let ib = Scanning.from_channel channel in
    let rec read_var i =
      let v = bscanf ib "%d" (fun v -> v) in
      if i != 0 then (i, v > 0)::(read_var (i+1)) else []
    in
    let vars = read_var 1 in
    let valuation = Array.make (List.length vars) false in
List.iter (function (i, v) -> valuation.(i) <- v) vars;
vars;;

let check_robdd valuation robdd =
  let rec visit = function
    | LeafFalse -> false
    | LeafTrue -> true
    | Node(Var(i), l, r) ->
      let v = if i > 0 then valuation.(abs i) else not valuation.(abs i) in
      if v then visit r else visit l
  in visit robdd;;
