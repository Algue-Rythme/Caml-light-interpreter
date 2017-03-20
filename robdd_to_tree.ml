open Dictionary;;
open Sifting_utils;;
open Dict_litHash;;

(* take a sifting object and gives the shared robdd + the list of the nodes *)
(* Note : only the nodes accessible by the root are created *)
let sift_to_robdd sift =
  let l = ref []
  and hashTbl = RobddHash.create sift.size in
  
  let rec tree_to_robdd tree = Printf.printf "%d -> " (TreeHash.find sift.node_int tree);
			       match tree with
    | LeafFalse_s -> print_string "LeafFalse\n";
       if not (RobddHash.mem hashTbl LeafFalse) then
	 l := LeafFalse::!l;
      RobddHash.replace hashTbl LeafFalse true;
      LeafFalse
    | LeafTrue_s -> print_string "LeafTrue\n";
       if not (RobddHash.mem hashTbl LeafTrue) then 
	 l := LeafTrue::!l;
       RobddHash.replace hashTbl LeafTrue true;
      LeafTrue
    | Node_s(Var(v), i, j) -> Printf.printf "Node(%d, %d, %d)\n" v i j;
      let fg = IntHash.find sift.int_node i in
      print_string "Fg found\n";
      let fd = IntHash.find sift.int_node j in
      print_string "Fd found\n";
       let n = Node(IntHash.find sift.renamingTable v,
		    tree_to_robdd fg,
		    tree_to_robdd fd) in
       if not (RobddHash.mem hashTbl n) then
	 l:= n::!l;
       RobddHash.replace hashTbl n true;
       n
  in
  print_string "Début génération\n";
  let t = tree_to_robdd (IntHash.find sift.int_node sift.root) in
  print_string "Fini !\n";
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
  LitHash.iter aux sift.lvlTable ;
  let hashTbl = RobddHash.create sift.size in
  let rec tree_to_robdd tree = let index = TreeHash.find sift.node_int tree in match tree with
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
       let n = Node(Var(index),
		    tree_to_robdd (IntHash.find sift.int_node i),
		    tree_to_robdd (IntHash.find sift.int_node j)) in
       if not (RobddHash.mem hashTbl n) then
	 l:= n::!l;
       RobddHash.replace hashTbl n true;
       n
  in
  let n = List.map tree_to_robdd (List.flatten !l1) in
  n, !l;

