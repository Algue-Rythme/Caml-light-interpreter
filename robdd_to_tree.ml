open Dictionary;;
open Sifting_utils;;

(* take a sifting object and gives the shared robdd + the list of the nodes *)
(* Note : only the nodes accessible by the root are created *)
let sift_to_robdd sift =
  let l = ref []
  and hashTbl = RobddHash.create sift.size in
  
  let rec tree_to_robdd tree = match tree with
    | LeafFalse_s ->
       if not (RobddHash.mem hashTbl LeafFalse) then
	 l := LeafFalse::!l;
      RobddHash.replace hashTbl LeafFalse true;
      LeafFalse
    | LeafTrue_s -> 
       if not (RobddHash.mem hashTbl LeafTrue) then 
	 l := LeafTrue::!l;
      RobddHash.add hashTbl LeafTrue true;
      LeafTrue
    | Node_s(Var(v), i, j) ->
       let n = Node(IntHash.find sift.renamingTable v,
		    tree_to_robdd (IntHash.find sift.int_node i),
		    tree_to_robdd (IntHash.find sift.int_node j)) in
       if not (RobddHash.mem hashTbl n) then
	 l:= n::!l;
       RobddHash.replace hashTbl n true;
       n
  in
  let t = tree_to_robdd (IntHash.find sift.int_node sift.root) in
  t, !l;;

(* Take a sifting object and gives the shared robdd + list of all nodes with the renaming *)
(* Note : only the nodes accessible by the root are created *)
let sift_to_robdd_renamed sift =
  let l = ref []
  and hashTbl = RobddHash.create sift.size in
  
  let rec tree_to_robdd tree = match tree with
    | LeafFalse_s ->
       if not (RobddHash.mem hashTbl LeafFalse) then
	 l := LeafFalse::!l;
      RobddHash.replace hashTbl LeafFalse true;
      LeafFalse
    | LeafTrue_s -> 
       if not (RobddHash.mem hashTbl LeafTrue) then 
	 l := LeafTrue::!l;
      RobddHash.add hashTbl LeafTrue true;
      LeafTrue
    | Node_s(v, i, j) ->
       let n = Node(v, tree_to_robdd (IntHash.find sift.int_node i),
		    tree_to_robdd (IntHash.find sift.int_node j)) in
       if not (RobddHash.mem hashTbl n) then
	 l:= n::!l;
       RobddHash.replace hashTbl n true;
       n
  in
  let t = tree_to_robdd (IntHash.find sift.int_node sift.root) in
  t, !l;;

(* Returns the list of the node in memory *)
let node_list_memory sift =
  let l1 = ref [] in
  let l = ref [] in
  let aux a b = l1:= b::!l1; in
  IntHash.iter aux sift.int_node;
  let hashTbl = RobddHash.create sift.size in
  let rec tree_to_robdd tree = match tree with
    | LeafFalse_s ->
       if not (RobddHash.mem hashTbl LeafFalse) then
	 l := LeafFalse::!l;
      RobddHash.replace hashTbl LeafFalse true;
      LeafFalse
    | LeafTrue_s -> 
       if not (RobddHash.mem hashTbl LeafTrue) then 
	 l := LeafTrue::!l;
      RobddHash.add hashTbl LeafTrue true;
      LeafTrue
    | Node_s(v, i, j) ->
       let n = Node(v,
		    tree_to_robdd (IntHash.find sift.int_node i),
		    tree_to_robdd (IntHash.find sift.int_node j)) in
       if not (RobddHash.mem hashTbl n) then
	 l:= n::!l;
       RobddHash.replace hashTbl n true;
       n
  in
  let n = List.map tree_to_robdd !l1 in
  n, !l;

