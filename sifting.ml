open Array;;
open Build_ROBDD;;
open Dict_litHash;;
open Dictionary;;

module OBDD_Build = ROBDD_BUILDER(ROBDD_LITHASH);; (* change here to select the dictionary implementation *)

(* Bien expliquer pourquoi j'ai fait quoi *)
type robdd_sifting = {
  mutable tree : robdd;
  mutable lvlTable : robdd list LitHash.t;
	mutable nodes : robdd list;
	mutable taille : int };;

let get_robdd sift = sift.tree;;

let make_robdd_sifting f =
  let tree, nodes = OBDD_Build.create f in
  let n = List.length nodes in
  let sift = {tree=tree; nodes; lvlTable=(LitHash.create 0); taille=n;} in
  let rec make_lvlTable list =
    let v0 = Var(0) in
    match list with
    | [] -> ()
    | x::q -> make_lvlTable q;
      match x with
      | Node(i, _, _) when LitHash.mem sift.lvlTable i ->
	 LitHash.replace sift.lvlTable i (x::(LitHash.find sift.lvlTable i))
      | Node(i, _, _) -> LitHash.replace sift.lvlTable i [x]
      |  LeafFalse | LeafTrue -> match LitHash.mem sift.lvlTable v0 with
	| false -> LitHash.replace sift.lvlTable v0 [x];
	| true -> LitHash.replace sift.lvlTable v0 (x::(LitHash.find sift.lvlTable v0));
  in
  make_lvlTable nodes;
  sift;;

let get_nodes sift =
  let l = ref [] in
  let aux a b = l := b@(!l) in
  LitHash.iter aux sift.lvlTable;
  !l;;


(*let get_nodes sift = sift.nodes;;*)

(* Indicate if a node is present in the robdd_sifting *)
let registered_node sift node = let v0 = Var(0) in match node with
  | LeafTrue when not (LitHash.mem sift.lvlTable v0) -> false
  | LeafFalse when not (LitHash.mem sift.lvlTable v0) -> false
  | LeafTrue | LeafFalse -> List.mem node (LitHash.find sift.lvlTable v0)
  | Node(i, _, _) when not (LitHash.mem sift.lvlTable i) -> false
  | Node(i, _, _) -> List.mem node (LitHash.find sift.lvlTable i);;


let free_node sift node = let v0 = Var(0) in
  let rec del_list l n = match l with
    | [] -> []
    | x::q when x=n -> del_list q n
    | x::q -> x::(del_list q n) in
  match node with
  | LeafTrue | LeafFalse -> LitHash.replace sift.lvlTable v0 (del_list (LitHash.find sift.lvlTable v0) node)
  | Node(x, _, _) -> LitHash.replace sift.lvlTable x (del_list (LitHash.find sift.lvlTable x) node)

(* swap i et i+1 *)
let swap sift i =
  let vi = Var(i) and vip1 = Var(i+1) in
  let listeNoeuds = LitHash.find sift.lvlTable vi in
  ()
