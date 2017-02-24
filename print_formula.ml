open Expr;;

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
  Printf.fprintf channel "}\n";
  flush channel;;
