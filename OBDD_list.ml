open Expr
open OBDD

(* dictionary by association list : bad perfs but fast to implement *)    
module OBDD_List : OBDD =
struct    
  type literal = Var of int
  type robdd = LeafFalse | LeafTrue | Node of literal * robdd * robdd

  let seen = ref [((0, LeafFalse, LeafFalse),LeafFalse); ((0, LeafTrue, LeafTrue), LeafTrue)] (* list of (prenode,node) *)
    
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
    in build f (literalsList f)

  let to_dot tree file =
    let i = ref 0 in
    let indexedNodeList = List.map (fun x -> incr i; (snd x, !i)) !seen in (* list of all nodes indexed by an int*)
    let channel = open_out file in
    let rec aux = function
      | [] -> ()
      | x::q -> aux q; let n, i = x in match n with
	| LeafFalse -> Printf.fprintf channel "%d [label = \"False\"];\n" i
	| LeafTrue -> Printf.fprintf channel "%d [label = \"True\"];\n" i
	| Node (Var(x), fg, fd) -> let ifg = List.assoc fg indexedNodeList and ifd = List.assoc fd indexedNodeList in
				   Printf.fprintf channel "%d [label = \"x%d\"];\n%d -> %d;\n%d -> %d;\n" i x i ifg i ifd;
    in
    Printf.fprintf channel "digraph BDD {\n";
    aux indexedNodeList;
    Printf.fprintf channel "}";
end
