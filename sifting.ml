open Sifting_utils;;
open Expr;;
open Dictionary;;
open Dict_litHash;;
open Print_formula;;


(* swap i and i+1 *)
let swap sift i =
  Printf.printf "Size before swap %d : %d\n" i sift.size;
  let vi = IntHash.find sift.lvlLitTable i and
      vip1 = IntHash.find sift.lvlLitTable (i+1) in
  IntHash.replace sift.lvlLitTable i vip1;
  IntHash.replace sift.lvlLitTable (i+1) vi;
  let listeNoeuds = LitHash.find sift.lvlTable vi in
  let swap_node node =
    let actualIndex = TreeHash.find sift.node_int node in
    (* we are going to replace the node in memory *) 
    match node with
    | LeafTrue_s | LeafFalse_s -> ()
    | Node_s(vi, indexFg, indexFd) ->
       let f11Index, f10Index = match IntHash.find sift.int_node indexFd with
	 | LeafTrue_s -> let ind = TreeHash.find sift.node_int LeafTrue_s in ind, ind
	 | LeafFalse_s -> let ind = TreeHash.find sift.node_int LeafFalse_s in ind, ind
	 | Node_s(_, low, high) -> high, low
       in
       let f00Index, f01Index = match IntHash.find sift.int_node indexFd with
	 | LeafTrue_s -> let ind = TreeHash.find sift.node_int LeafTrue_s in ind, ind
	 | LeafFalse_s -> let ind = TreeHash.find sift.node_int LeafFalse_s in ind, ind
	 | Node_s(_, low, high) -> high, low
       in
       let highIndex =
	 if f11Index = f01Index then
	   f11Index
	 else
	   add_node_if_not_present sift (Node_s(vi, f01Index, f11Index))
       in
       let lowIndex =
	 if f10Index = f00Index then
	   f00Index
	 else
	   add_node_if_not_present sift (Node_s(vi, f00Index, f10Index))
       in
       updateIndex sift actualIndex (Node_s(vip1, lowIndex, highIndex))
  in
  let rec aux l = match l with
    | [] -> ()
    | x::q -> swap_node x;
      aux q;
  in
  aux listeNoeuds;
  Printf.printf "Size after swap %d : %d\n" i sift.size;
;;
