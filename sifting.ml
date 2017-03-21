open Sifting_utils;;
open Expr;;
open Dictionary;;
open Dict_litHash;;
open Print_formula;;

open Printf;;

(* swap i and i+1 *)
let swap sift i =
  let vi = IntHash.find sift.lvlLitTable i and
      vip1 = IntHash.find sift.lvlLitTable (i+1) in
  IntHash.replace sift.lvlLitTable i vip1;
  IntHash.replace sift.lvlLitTable (i+1) vi;
  let listeNoeuds = LitHash.find sift.lvlTable vi in
  let node_to_free = ref [] in
  let node_not_to_free = ref [] in
  let swap_node node =
    let actualIndex = TreeHash.find sift.node_int node in
    (* we are going to replace the node in memory *) 
    match node with
    | LeafTrue_s | LeafFalse_s -> ()
    | Node_s(vi, indexFg, indexFd) ->
       (* the two n+1 index high *)
       let f11Index, f10Index = match IntHash.find sift.int_node indexFd with
	 | LeafTrue_s | LeafFalse_s ->  indexFd, indexFd
	 | Node_s(x, _, _) when x <> vip1 -> indexFd, indexFd
	 | Node_s(_, low, high) ->
	    node_to_free := (IntHash.find sift.int_node indexFd)::!node_to_free;
	   high, low
       in
       (* the two n+1 index low *)
       let f01Index, f00Index = match IntHash.find sift.int_node indexFg with
	 | LeafTrue_s | LeafFalse_s -> indexFg, indexFg
	 | Node_s(x, _, _) when x <> vip1 -> indexFg, indexFg
	 | Node_s(_, low, high) -> 
	    node_to_free := (IntHash.find sift.int_node indexFg)::!node_to_free;
	   high, low
       in
       let highIndex =
	 if f11Index = f01Index then
	   f11Index
	 else
	   add_node_if_not_present sift (Node_s(vi, f01Index, f11Index))
       in
       node_not_to_free := (IntHash.find sift.int_node highIndex)::!node_not_to_free;
       let lowIndex =
	 if f10Index = f00Index then
	   f00Index
	 else
	   add_node_if_not_present sift (Node_s(vi, f00Index, f10Index))
       in
       node_not_to_free := (IntHash.find sift.int_node lowIndex)::!node_not_to_free;

       let newNode = if (lowIndex = highIndex) then
	   IntHash.find sift.int_node lowIndex
	 else(
	   Node_s(vip1, lowIndex, highIndex)) in
       updateIndex sift actualIndex newNode;
  in

  let rec aux l = match l with
    | [] -> ()
    | x::q -> swap_node x; aux q;
  in
  aux listeNoeuds;
  
  let rec free_mem list = match list with
    | [] -> ()
    | LeafFalse_s::q -> free_mem q
    | LeafTrue_s::q -> free_mem q
    | x::q when List.mem x !node_not_to_free -> free_mem q;
    | x::q -> free_node sift x; free_mem q;
  in
free_mem !node_to_free;
;;

let sifting sift =
  for i = 1 to sift.nb_lit do
    let jMin = ref i and sizeMin = ref sift.size in
    for j = i to sift.nb_lit-1 do
      swap sift j;
      if sift.size < !sizeMin then (
	jMin := j;
	sizeMin := sift.size;
      )
    done;
    for j = sift.nb_lit-1 downto 1 do
      swap sift j;
      if sift.size < !sizeMin then (
	jMin := j;
	sizeMin := sift.size;
      )
    done;
    for j = 1 to !jMin-1 do
      swap sift j;
    done;
  done;;
