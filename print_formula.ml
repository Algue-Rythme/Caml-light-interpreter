open Expr;;

let printPropFormula expr =
  let rec binOp left right =
    begin
      print_string "(";
      aux left;
      print_string ", ";
      aux right;
      print_string ")";
    end
  and aux = function
    | Const(c) -> print_string (if c then "True" else "False")
    | Literal(x) -> print_int x
    | Not(a) -> print_string "Not("; aux a; print_string ")"
    | And(a, b) -> print_string "And"; binOp a b
    | Or(a, b) -> print_string "Or"; binOp a b
    | Xor(a, b) -> print_string "Xor"; binOp a b
    | Implies(a, b) -> print_string "Implies"; binOp a b
    | Equivalent(a, b) -> print_string "Equivalent"; binOp a b
  in aux expr; print_newline ();;

open Dictionary;;
open Printf;;

(*
print the robdd into dot file
dotted edges for "false path" and full edges for "true path"
*)
let tree_to_dot nodes file =
  let i = ref 0 in
  let indexedNodeList = List.map (fun node -> incr i; (node, !i)) nodes in (* list of all nodes indexed by an int*)
  let channel = open_out file in
  let printf s = fprintf channel (format_of_string s) in
  let rec aux = function
    | [] -> ()
    | x::q -> aux q;
      let n, i = x in
      match n with
        | LeafFalse -> printf "tree_%d [label = \"False\"];\n" i
        | LeafTrue -> printf "tree_%d [label = \"True\"];\n" i
        | Node (Var(x), fg, fd) ->
          let ifg = List.assoc fg indexedNodeList and ifd = List.assoc fd indexedNodeList in
          printf "tree_%d [label = \"x%d\"];\ntree_%d -> tree_%d [style=dotted];\ntree_%d -> tree_%d;\n" i x i ifg i ifd;
  in
  printf "digraph ROBDD {\n";
  aux indexedNodeList;
  printf "}\n\n";
  close_out channel;;

(*
Print the formula into a dot file
The => operator is not symetric, so dotted edges are used to show the direction of implication
*)
let prop_to_dot formula file =
  let channel = open_out file in
  let printf s = fprintf channel (format_of_string s) in
  let i = ref 1 in
  let rec aux father f =
    let cur = !i in
    fprintf channel "node_%d -> node_%d;\nnode_%d [label = " cur father cur;
    incr i;
    let _ = match f with
      | Const(true) -> printf "\"True\"];;\n"; -1
      | Const(false) -> printf "\"False\"];\n"; -1
      | Literal(x) -> printf "\"x%d\"];\n" x; -1
      | Not(x) -> printf "\"~\"];\n"; aux cur x
      | And(a, b) -> printf "\"&&\"];\n"; let _ =  aux cur a in aux cur b
      | Or(a, b) -> printf "\"||\"];\n"; let _ = aux cur a in aux cur b
      | Xor(a, b) -> printf "\"X\"];\n"; let _ = aux cur a in aux cur b
      | Implies(a, b) ->
        fprintf channel "\"=>\"];\n";
        let hypothesis = aux cur a in
        let consequence = aux cur b in
        printf "node_%d -> node_%d [style=dotted];\n" hypothesis consequence; -1
      | Equivalent(a, b) -> printf "\"<=>\"];\n"; let _ = aux cur a in aux cur b
    in cur
  in
  printf "digraph Formula {\n";
  printf "node_0 [label = \"Out\"];\n";
  let _ = aux 0 formula in
  printf "}\n\n";
  close_out channel;;

open Tseitin;;

(* a little complicated function just to count the number of literals in CNF, because of hard typing *)
let nb_var_cnf cnf = List.length (
    List.sort_uniq (fun a b -> (abs a)-(abs b))
      (List.map dezip_literal
         (List.concat (List.map dezip_clause (dezip_cnf cnf)))
      )
  );;

(* print CNF into the file that goes into minisat input *)
let printCNF cnf file =
  let channel = open_out file in
  let printf s = fprintf channel (format_of_string s) in
  printf "p cnf %d %d\n" (nb_var_cnf cnf) (List.length (dezip_cnf cnf));
  let printClause = List.iter (function CNF_Literal(l) -> printf "%d " l) in
  let printClauses = List.iter (function Clause(lits) -> printClause lits; printf "0\n") in
  printClauses (dezip_cnf cnf);
  close_out channel;;
