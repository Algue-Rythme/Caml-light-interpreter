(* by convention always write code about operators sorted by priority *)

type propFormula =
    Const of int
  | Not of propFormula
  | And of propFormula * propFormula
  | Or of propFormula * propFormula
  | Xor of propFormula * propFormula
  | Implies of propFormula * propFormula
  | Equivalent of propFormula * propFormula;;

(* A module to represent and update valuation *)

exception UnknownVariable;;
module Valuation =
struct
  type valuation = bool array ref
  let create affectation = ref affectation
  let request valuation x =
    let y = if x > 0 then x else -x in
    if (y = 0 || y > Array.length (!valuation))
    then raise UnknownVariable
    else (!valuation).(y-1)
  let set valuation x v =
    let y = if x > 0 then x else -x in
    if (y = 0 || y > Array.length (!valuation))
    then raise UnknownVariable
    else (!valuation).(y) = v
end

(*
  Convention :
    1) x is always a name for variable
    2) "a" and "b", or "left" and "right" are name for propFormula
*)

let evalPropFormula valuation prop =
  (* eval benefits of ocaml lazy evaluation of conditions*)
  let rec eval = function
    | Const(x) -> Valuation.request valuation x (* Deal with negative x *)
    | Not(a) -> eval a
    | And(a, b) -> (eval a) && (eval b)
    | Or(a, b) -> (eval a) || (eval b)
    | Xor(a, b) -> if (eval a) then not (eval b) else eval b
    | Implies(a, b) -> if (eval a) then (eval b) else true
    | Equivalent(a, b) -> (eval a) = (eval b)
  in eval prop;;

let rec printPropFormula expr =
  let binOp left right =
    begin
      print_string "(";
      printPropFormula left;
      print_string ", ";
      printPropFormula right;
      print_string ")";
    end
  in match expr with
  | Const(x) -> print_int x
  | Not(a) -> print_string "Not("; printPropFormula a; print_string ")"
  | And(a, b) -> print_string "And"; binOp a b
  | Or(a, b) -> print_string "Or"; binOp a b
  | Xor(a, b) -> print_string "Xor"; binOp a b
  | Implies(a, b) -> print_string "Implies"; binOp a b
  | Equivalent(a, b) -> print_string "Equivalent"; binOp a b
;;
