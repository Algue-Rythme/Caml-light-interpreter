open Expr;;

type literal = Var of int;;
type robdd = LeafFalse | LeafTrue | Node of literal * robdd * robdd;;

open Hashtbl;;

(* TODO : implement ROBDDHash *)

type MapROBDD = Hashtbl.Make(ROBDDHash);;

let buildROBDD f =
  let tbl = MapROBDD.create 2 in
  let mk var low high =
    let prenode = (var, low, high) in
    if low = high then low (* redundant test *)
    else if MapROBDD.mem tbl prenode then MapROBDD.find tbl prenode (* sharing *)
    else (
      let node = Node(var, low, high) (* create new node *)
      in MapROBDD.add tbl prenode node; node
    )
  in
  let rec build formula i =
    if i > n then ( if eval formula then LeafTrue else LeafFalse )
    else (
      let v0 = build (replace formula i false) in
      let v1 = build (replace formula i true) in
      mk i v0 v1
    )
  in build f 1;;
