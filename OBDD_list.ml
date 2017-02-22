open Expr
open OBDD

(* dictionary by association list -> bad but fast way to implement *)    
module OBDD_List : OBDD =
struct    
  type literal = Var of int
  type robdd = LeafFalse | LeafTrue | Node of literal * robdd * robdd

  let seen = ref [] 
    
  let create f = 
    let mk var low high =
      let prenode = (var, low, high) in
      if low = high then low (* redundant test *)
      else if List.mem_assoc prenode (!seen) then
	(* sharing if we already saw this node *)
	List.assoc prenode (!seen)
      else (
	let node = Node(Var(var), low, high) in (* create new node *)
	seen := (prenode, node)::(!seen);
	node
      )
    in
    
    let rec build formula l = match l with
      | [] -> if eval formula then LeafTrue else LeafFalse
      | i::q -> 
	 let v0 = build (replace formula i false) q in
	 let v1 = build (replace formula i true) q in
	 mk i v0 v1
    in build f (literalsList f);;

end
