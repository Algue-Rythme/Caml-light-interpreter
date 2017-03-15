open Dictionary;;
open Expr;;

(*
  Functor to create ROBDD with a propFormula,
  take a dictionary as argument to choose the efficiency of the implementation 
*)

module ROBDD_BUILDER =
  functor (Dict : Dictionary) ->
    struct
      let create f =
        let tbl = Dict.create () in
        let mk var low high =
          let node = Node(Var(var), low, high) in
          if low = high then low (* redundant test *)
          else if Dict.mem tbl node then Dict.find tbl node (* sharing *)
          else ( Dict.add tbl node; node ) (* create new node *)
        in
        let rec build formula = function
          | [] ->
            let leaf = if eval formula then LeafTrue else LeafFalse in
            if Dict.mem tbl leaf then Dict.find tbl leaf else ( Dict.add tbl leaf; leaf )
          | lit::q ->
            let v0 = build (replace formula lit false) q in
            let v1 = build (replace formula lit true) q in
            mk lit v0 v1
        in
        let tree = build f (literalsList f) in
        (tree, Dict.to_list tbl)
        (* return structured set of node (robdd) and unstructured (Dict) without doublons *)
    end;;
