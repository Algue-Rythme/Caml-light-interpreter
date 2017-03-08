open Array;;
open Build_ROBDD;;
open Dict_litHash;;
open Dictionary;;

module OBDD_Build = ROBDD_BUILDER(ROBDD_LITHASH);; (* change here to select the dictionary implementation *)

type robdd_sifting = { mutable tree : robdd;
		       mutable nodes : robdd list;
		       mutable lvlTable : robdd list LitHash.t;
		       taille : int };;

let make_robdd_sifting f =    
  let tree, nodes = OBDD_Build.create f in
  let n = List.length nodes in
  let sift = {tree=tree; nodes=nodes; lvlTable=(LitHash.create 0); taille=n;} in
  let rec make_lvlTable list = match list with
    | [] -> ()
    | x::q -> make_lvlTable q;
      match x with
      | Node(i, _, _) when LitHash.mem sift.lvlTable i ->
	 LitHash.add sift.lvlTable i (x::(LitHash.find sift.lvlTable i))
      | Node(i, _, _) -> LitHash.add sift.lvlTable i [x]
      | _ -> failwith "Error in make_robdd_sifting" in
  make_lvlTable nodes;
  sift;;

