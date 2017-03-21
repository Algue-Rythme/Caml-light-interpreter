open Array;;
open Build_ROBDD;;
open Dict_litHash;;
open Dictionary;;
open Expr;;

module OBDD_Build = ROBDD_BUILDER(ROBDD_LITHASH);; (* change here to select the dictionary implementation *)

type tree_sifting = LeafTrue_s | LeafFalse_s | Node_s of literal * int * int;;

module HashTree =
struct
  type t = tree_sifting
  let equal a b = (a=b)
  let hash x = Hashtbl.hash x
end

module HashRobdd =
struct
  type t = robdd
  let equal a b = (a=b)
  let hash x = Hashtbl.hash x
end

module HashInt =
struct
  type t = int
  let equal a b = (a=b)
  let hash x = Hashtbl.hash x
end

module IntHash = Hashtbl.Make(HashInt)
module TreeHash = Hashtbl.Make(HashTree)
module RobddHash = Hashtbl.Make(HashRobdd)

(* New type for the sifting : each int represent a pointer to the child *)

  
type robdd_sifting = {

  mutable root : int; (* the index of the root of the tree *)
  
  mutable lvlTable : tree_sifting list LitHash.t; (* the table of the differents levels of the DAG*)
  mutable size : int; (* size of the DAG, may evolve with swapping *)

  mutable renamingTable : literal IntHash.t; (* a hashtable for renaming of variables*)
  mutable nameLit : int LitHash.t;

  mutable lvlLitTable : literal IntHash.t; (* a map to store where each variable is *)

  nb_lit : int; (* the number of literals, for the sifting*)
  
  (* memory emulation *)
  mutable node_int : int TreeHash.t; (* mapping node -> index *)
  mutable int_node : tree_sifting IntHash.t; (* mapping index -> node *)
  mutable mem_offset : int; (* the next integer for the mapping *)
  mutable avaible_index : int list; (* List of removed index for garbage collecting *)
  mutable number_link_to : int IntHash.t; (* number of link to a given index in mem *)
} ;;

(* final tree built at the very end of the process *)  

let make_robdd_sifting f =
  let tree, nodes = OBDD_Build.create f in
  let n = List.length nodes in
  let lvlTable = LitHash.create 0 in
  let nameTable = IntHash.create 0 and
      actualName = ref 1 and
      nameLit = LitHash.create 0
  in
  let lvlLitTable = IntHash.create 0 in
  let nodeList = ref [] in
  let node_int = TreeHash.create n and
      int_node = IntHash.create n and
      robdd_int = RobddHash.create n and
      number_link_to = IntHash.create n 
  in
  let required_index = IntHash.create n in
  
  (* give an adress in memory for every node *)
  let rec index_node list c = match list with
    | [] -> ()
    | x::q -> 
       RobddHash.replace robdd_int x c;
      index_node q (c+1) in
  (* register the node in memory (conversion and renaming) *)
  let rec make_sifting_mem list = match list with
    | [] -> ()
    | x::q -> make_sifting_mem q;
      let i = RobddHash.find robdd_int x in
      match x with
      | LeafTrue ->
	 IntHash.replace int_node i LeafTrue_s;
	TreeHash.replace node_int LeafTrue_s i;
	nodeList := LeafTrue_s::!nodeList;
	
      | LeafFalse ->
	 IntHash.replace int_node i LeafFalse_s;
	TreeHash.replace node_int LeafFalse_s i;
	nodeList := LeafFalse_s::!nodeList;
	
      | Node(Var(v), fg, fd) -> 
	 let g = RobddHash.find robdd_int fg and
	     d = RobddHash.find robdd_int fd in
	 if not (IntHash.mem number_link_to g) then
	   IntHash.replace number_link_to g 1
	 else
	   IntHash.replace number_link_to g ((IntHash.find number_link_to g)+1);
	 
	 if not(IntHash.mem number_link_to d) then
	   IntHash.replace number_link_to d 1
	 else
	   IntHash.replace number_link_to d ((IntHash.find number_link_to d)+1);
	 
	 let node =
	   if not(LitHash.mem nameLit (Var(v)) ) then (
	     IntHash.replace nameTable !actualName (Var(v));
	     LitHash.replace nameLit (Var(v)) !actualName;
	     incr actualName;
	     Node_s(Var(!actualName-1), g, d)
	   )
	   else (
	     let name = LitHash.find nameLit (Var(v)) in
	     Node_s(Var(name), g, d)
	   )
	 in
	 IntHash.replace int_node i node;
	 TreeHash.replace node_int node i;
	 nodeList := node::!nodeList;
	 
  in
  (* Create the lvl table (list of all the node for a given litteral of the tree) *)
  let rec make_lvlTable list =
    let v0 = Var(0) in
    match list with
    | [] -> ()
    | x::q -> make_lvlTable q;
      match x with
      | Node_s(i, _, _) when LitHash.mem lvlTable i ->
	 LitHash.replace lvlTable i (x::(LitHash.find lvlTable i)); (* Append the list*)
	
      | Node_s(i, _, _) -> LitHash.replace lvlTable i [x]; (* create the list *)
	let vi = match i with Var(vi)->vi in
	IntHash.replace lvlLitTable vi i; (* the ith lit is at level i in the dag *)
	
      |  LeafFalse_s | LeafTrue_s -> match LitHash.mem lvlTable v0 with
	| false -> LitHash.replace lvlTable v0 [x]
	| true -> LitHash.replace lvlTable v0 (x::(LitHash.find lvlTable v0))
  in
  index_node nodes 0;
  let nodes_2 = List.rev (List.sort Pervasives.compare nodes) in
  make_sifting_mem nodes_2; (* list sorted to keep the order on the variables *)
  make_lvlTable !nodeList;
  let s = {root=(RobddHash.find robdd_int tree); lvlTable=lvlTable; size=n;
	   renamingTable=nameTable;nameLit; lvlLitTable; nb_lit=(!actualName-1);node_int; int_node;
	   mem_offset=n; avaible_index=[]; number_link_to}
  in
  s;;

(* Indicate if a node is present in the robdd_sifting *)
let registered_node sift node = TreeHash.mem sift.node_int node;;

(* Remove a node in the robdd_sifting (not freed !)*)
let remove_node sift node =
  let v0 = Var(0) in
  let index = TreeHash.find sift.node_int node in
  TreeHash.remove sift.node_int node; (* remove from the memory *)
  IntHash.remove sift.int_node index; (* here, the index is unused but not avaible*)
  let rec del_list l n = match l with
    | [] -> []
    | x::q when x=n -> del_list q n
    | x::q -> x::(del_list q n) in
  match node with (* remove from the lvlTable *)
  | LeafTrue_s | LeafFalse_s ->
     LitHash.replace sift.lvlTable v0 (del_list (LitHash.find sift.lvlTable v0) node)
  | Node_s(x, indexLow, indexHigh) ->
     IntHash.replace sift.number_link_to indexLow ((IntHash.find sift.number_link_to indexLow)-1);
     IntHash.replace sift.number_link_to indexHigh ((IntHash.find sift.number_link_to indexHigh)-1);
     LitHash.replace sift.lvlTable x (del_list (LitHash.find sift.lvlTable x) node)
       
(* Free the memory for the node *)
let free_node sift node =
  if not (TreeHash.mem sift.node_int node) then ()
  else let index = TreeHash.find sift.node_int node in
       if IntHash.mem sift.number_link_to index &&
	 IntHash.find sift.number_link_to index <> 0 then ()
  else(
    sift.size <- sift.size-1; (* the size of the sift is decreased *)

    sift.avaible_index <- index::sift.avaible_index; (*a new index is avaible*)
    remove_node sift node);; (* now we remove the node*)

(* update an index to replace a node*)
let updateIndex sift index node =
  let oldNode = IntHash.find sift.int_node index in (* the old node is removed *)
  remove_node sift oldNode;
  IntHash.replace sift.int_node index node; (* the new node is added*)
  TreeHash.replace sift.node_int node index;
  match node with (* now the new node is added to the lvlTable *)
  | Node_s(x, i, j) ->
     if not (IntHash.mem sift.number_link_to i) then
	 IntHash.replace sift.number_link_to i 1
       else
	 IntHash.replace sift.number_link_to i ((IntHash.find sift.number_link_to i)+1);
      
      if not (IntHash.mem sift.number_link_to j) then
	 IntHash.replace sift.number_link_to j 1
       else
	 IntHash.replace sift.number_link_to j ((IntHash.find sift.number_link_to j)+1);
      LitHash.replace sift.lvlTable x (node::(LitHash.find sift.lvlTable x))
  | _ -> () (* no interest to add a Leaf *)
;;

(* add a node if it is not already present, and return its index *)
let add_node_if_not_present sift node =
  if registered_node sift node then
    TreeHash.find sift.node_int node 
  else begin
    sift.size <- (sift.size + 1);
    match node with
    | LeafFalse_s | LeafTrue_s -> failwith "Adding a Leaf" (* no interest in adding a Leaf *)
    | Node_s(v, i, j) -> (* add the node to the lvlTable *)
       if not (IntHash.mem sift.number_link_to i) then
	 IntHash.replace sift.number_link_to i 1
       else
	 IntHash.replace sift.number_link_to i ((IntHash.find sift.number_link_to i)+1);
      
      if not (IntHash.mem sift.number_link_to j) then
	 IntHash.replace sift.number_link_to j 1
       else
	 IntHash.replace sift.number_link_to j ((IntHash.find sift.number_link_to j)+1);

       if LitHash.mem sift.lvlTable v then
	 LitHash.replace sift.lvlTable v (node::(LitHash.find sift.lvlTable v))
       else
	 LitHash.replace sift.lvlTable v [node];
      (* add the node in memory *)
      match sift.avaible_index with
      | [] -> (* the memory is full*)
	 let index = sift.mem_offset in
	 sift.mem_offset <- sift.mem_offset+1; (* we increase the offset*)
	 IntHash.replace sift.int_node index node;
	 TreeHash.replace sift.node_int node index;
	 index
      | index::q -> (* there are freed cases in memory*)
	 sift.avaible_index <- q;
	IntHash.replace sift.int_node index node;
	TreeHash.replace sift.node_int node index;
	index
  end
