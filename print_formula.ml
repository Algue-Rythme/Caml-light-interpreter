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
open Printf;;

let tree_to_dot nodes file =
  let i = ref 0 in
  let indexedNodeList = List.map (fun node -> incr i; (node, !i)) nodes in (* list of all nodes indexed by an int*)
  let channel = open_out file in
  let rec aux = function
    | [] -> ()
    | x::q -> aux q;
      let n, i = x in
      match n with
        | LeafFalse -> fprintf channel "tree_%d [label = \"False\"];\n" i
        | LeafTrue -> fprintf channel "tree_%d [label = \"True\"];\n" i
        | Node (Var(x), fg, fd) ->
          let ifg = List.assoc fg indexedNodeList and ifd = List.assoc fd indexedNodeList in
          fprintf channel "tree_%d [label = \"x%d\"];\ntree_%d -> tree_%d [style=dotted];\ntree_%d -> tree_%d;\n" i x i ifg i ifd;
  in
  fprintf channel "digraph ROBDD {\n";
  aux indexedNodeList;
  fprintf channel "}\n\n";
  flush channel;;

open StdLabels;;

let prop_to_dot formula file =
  let channel = open_out file in
  let i = ref 1 in
  let rec aux father f =
    let cur = !i in
    fprintf channel "node_%d -> node_%d;\nnode_%d [label = " cur father cur;
    incr i;
    let _ = match f with
      | Const(true) -> fprintf channel "\"True\"];;\n"; -1
      | Const(false) -> fprintf channel "\"False\"];\n"; -1
      | Literal(x) -> fprintf channel "\"x%d\"];\n" x; -1
      | Not(x) -> fprintf channel "\"~\"];\n"; aux cur x
      | And(a, b) -> fprintf channel "\"&&\"];\n"; aux cur a; aux cur b
      | Or(a, b) -> fprintf channel "\"||\"];\n"; aux cur a; aux cur b
      | Xor(a, b) -> fprintf channel "\"X\"];\n"; aux cur a; aux cur b
      | Implies(a, b) ->
        fprintf channel "\"=>\"];\n";
        let hypothesis = aux cur a in
        let consequence = aux cur b in
        fprintf channel "node_%d -> node_%d [style=dotted];\n" hypothesis consequence; -1
      | Equivalent(a, b) -> fprintf channel "\"<=>\"];\n"; aux cur a; aux cur b
    in cur
  in
  fprintf channel "digraph Formula {\n";
  fprintf channel "node_0 [label = \"Out\"];\n";
  aux 0 formula;
  fprintf channel "}\n\n";
  flush channel;;
