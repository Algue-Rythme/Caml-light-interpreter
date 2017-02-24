(* by convention always write code about operators sorted by priority *)

type propFormula =
    Const of bool
  | Literal of int
  | Not of propFormula
  | And of propFormula * propFormula
  | Or of propFormula * propFormula
  | Xor of propFormula * propFormula
  | Implies of propFormula * propFormula
  | Equivalent of propFormula * propFormula;;

(*
  Convention :
    1) x is always a name for variable
    2) c is always a name for a constant
    3) "a" and "b", or "left" and "right" are name for propFormula
*)

(* Replace the literal i and -i by value in formula *)
let replace formula i value =
  let rec aux = function
    | Const(c) -> Const(c)
    | Literal(x) when x = i -> Const(value)
    | Literal(x) when x = -i -> Const(not value)
    | Literal(x) -> Literal(x)
    | Not(a) -> Not(aux a)
    | And(a, b) -> And(aux a, aux b)
    | Or(a, b) -> Or(aux a, aux b)
    | Xor(a, b) -> Xor(aux a, aux b)
    | Implies(a, b) -> Implies(aux a, aux b)
    | Equivalent(a, b) -> Equivalent(aux a, aux b)
  in aux formula;;

(* replace literals in the proposition according to the domain *)
let rec replaceLiterals formula = function
  | [] -> formula
  | (lit, value)::q -> replaceLiterals (replace formula lit value) q;;

(* eval a proposition without literal *)
exception UnknownVariable of int;;
let rec eval = function
  | Const(c) -> c
  | Literal(x) -> raise (UnknownVariable(abs x))
  | Not(a) -> eval a
  | And(a, b) -> (eval a) && (eval b)
  | Or(a, b) -> (eval a) || (eval b)
  | Xor(a, b) -> if (eval a) then not (eval b) else eval b
  | Implies(a, b) -> if (eval a) then (eval b) else true
  | Equivalent(a, b) -> (eval a) = (eval b);;

(* returns a ordered list of literals (all positives) *)
let rec literalsList = function
  | Const(_) -> []
  | Literal(i) -> [abs i]
  | Not(a) -> literalsList a
  | And(a, b) ->
     List.merge (fun a b -> a-b) (literalsList a) (literalsList b)
  | Or(a, b) ->
     List.merge (fun a b -> a-b) (literalsList a) (literalsList b)
  | Xor(a, b) ->
     List.merge (fun a b -> a-b) (literalsList a) (literalsList b)
  | Implies(a, b) ->
     List.merge (fun a b -> a-b) (literalsList a) (literalsList b)
  | Equivalent(a, b) ->
    List.merge (fun a b -> a-b) (literalsList a) (literalsList b)
;;

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
  | Const(c) -> print_string (if c then "True" else "False")
  | Literal(x) -> print_int x
  | Not(a) -> print_string "Not("; printPropFormula a; print_string ")"
  | And(a, b) -> print_string "And"; binOp a b
  | Or(a, b) -> print_string "Or"; binOp a b
  | Xor(a, b) -> print_string "Xor"; binOp a b
  | Implies(a, b) -> print_string "Implies"; binOp a b
  | Equivalent(a, b) -> print_string "Equivalent"; binOp a b
;;

open Dictionary;;

let to_dot nodes file =
  let i = ref 0 in
  let indexedNodeList = List.map (fun node -> incr i; (node, !i)) nodes in (* list of all nodes indexed by an int*)
  let channel = open_out file in
  let rec aux = function
    | [] -> ()
    | x::q -> aux q;
      let n, i = x in
      match n with
        | LeafFalse -> Printf.fprintf channel "%d [label = \"False\"];\n" i
        | LeafTrue -> Printf.fprintf channel "%d [label = \"True\"];\n" i
        | Node (Var(x), fg, fd) ->
          let ifg = List.assoc fg indexedNodeList and ifd = List.assoc fd indexedNodeList in
          Printf.fprintf channel "%d [label = \"x%d\"];\n%d -> %d [style=dotted];\n%d -> %d;\n" i x i ifg i ifd;
  in
  Printf.fprintf channel "digraph BDD {\n";
  aux indexedNodeList;
  Printf.fprintf channel "}";
  flush channel;;
