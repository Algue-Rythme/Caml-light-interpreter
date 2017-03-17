open Dictionary;;

module HashLit =
struct
  type t = literal
  let equal a b = (a=b)
  let hash x = match x with
	   | Var(a) -> a 
end

module LitHash = Hashtbl.Make(HashLit);;

(* TODO : commenter *)

module ROBDD_LITHASH =
struct
  type dict = robdd list LitHash.t * (robdd list ref)
  let create () = LitHash.create 0 , (ref [])
  let v0 = Var(0)
  let mem dict node = let dict_list, _ = dict in match node with
    | Node(i, _, _) when not (LitHash.mem dict_list i) -> false
    | Node(i, _, _) -> List.mem node (LitHash.find dict_list i)
    | LeafFalse when not (LitHash.mem dict_list v0) -> false
    | LeafTrue when not (LitHash.mem dict_list v0) -> false
    | LeafTrue | LeafFalse -> List.mem node (LitHash.find dict_list v0)
       
  let find dict node = let dict_list, _ = dict in match node with
    | Node(i, _, _) when LitHash.mem dict_list i->
       List.find (fun other -> node = other) (LitHash.find dict_list i)
    | LeafFalse when LitHash.mem dict_list v0 ->
       List.find (fun other -> node = other) (LitHash.find dict_list v0)
    | LeafTrue when LitHash.mem dict_list v0 ->
       List.find (fun other -> node = other) (LitHash.find dict_list v0)
    | _ -> failwith "Trying to find something not in a litMap"

  let add dict node = let dict_list, liste = dict in match node with
    | Node(i, _, _) when LitHash.mem dict_list i ->
       LitHash.replace dict_list i (node::(LitHash.find dict_list i)); liste := node::!liste;
    | Node(i, _, _) ->
       LitHash.replace dict_list i [node]; liste := node::!liste;
    | LeafFalse | LeafTrue -> match LitHash.mem dict_list v0 with
      | false -> LitHash.replace dict_list v0 [node];
	liste := node::!liste;
      | true -> LitHash.replace dict_list v0 (node::(LitHash.find dict_list v0));
	liste := node::!liste
       
  let to_list dict =
    let _, liste = dict in !liste
end
