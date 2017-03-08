open Array;;
open Build_ROBDD;;
open Dict_litMap;;
open Dictionary;;

module OBDD_List = ROBDD_BUILDER(ROBDD_LITMAP);; (* change here to select the dictionary implementation *)

type robdd_sifting = { mutable tree : robdd;
		       mutable nodes : robdd list;
		       mutable lvlTable : robdd list LitMap.t;
		       taille : int };;

let make_robdd_sifting f =    
  let tree, nodes = OBDD_List.create f in
  let n = List.length nodes in
  let sift = {tree=tree; nodes=nodes; lvlTable=LitMap.empty; taille=n;} in
  let rec make_lvlTable list = match list with
    | [] -> ()
    | x::q -> make_lvlTable q;
      match x with
      | Node(i, _, _) when LitMap.mem i sift.lvlTable ->
	 sift.lvlTable <- (LitMap.add i (x::(LitMap.find i sift.lvlTable)) sift.lvlTable)
      | Node(i, _, _) ->
	 sift.lvlTable <- LitMap.add i [x] sift.lvlTable
      | _ -> failwith "Error in make_robdd_sifting" in
  make_lvlTable nodes;
  sift;;

