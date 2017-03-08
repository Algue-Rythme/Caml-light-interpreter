open Dictionary;;

module HashLit =
struct
  type t = literal
  let compare a b = match a, b with
    | Var(i), Var(j) -> i-j
end

module LitMap = Map.Make(HashLit);;

module ROBDD_LITMAP =
struct
  type dict = robdd list LitMap.t ref
  let create () = ref LitMap.empty
  let v0 = Var(0)
  let mem dict_list node = match node with
    | Node(i, _, _) when not (LitMap.mem i !dict_list) -> false
    | Node(i, _, _) -> List.mem node (LitMap.find i !dict_list)
    | LeafFalse when not (LitMap.mem v0 !dict_list) -> false
    | LeafTrue when not (LitMap.mem v0 !dict_list) -> false
    | LeafTrue | LeafFalse -> List.mem node (LitMap.find v0 !dict_list)
       
  let find dict_list node = match node with
    | Node(i, _, _) when LitMap.mem i !dict_list ->
       List.find (fun other -> node = other) (LitMap.find i !dict_list)
    | LeafFalse when LitMap.mem v0 !dict_list ->
       List.find (fun other -> node = other) (LitMap.find v0 !dict_list)
    | LeafTrue when LitMap.mem v0 !dict_list ->
       List.find (fun other -> node = other) (LitMap.find v0 !dict_list)
    | _ -> failwith "Trying to find something not in a litMap"

  let add dict_list node = match node with
    | Node(i, _, _) when LitMap.mem i !dict_list ->
       dict_list := (LitMap.add i (node::(LitMap.find i !dict_list)) !dict_list)
    | Node(i, _, _) ->
       dict_list := (LitMap.add i [node] !dict_list)
    | LeafFalse | LeafTrue -> match LitMap.mem v0 !dict_list with
      | false -> dict_list := LitMap.add v0 [node] !dict_list
      | true -> dict_list := LitMap.add v0 (node::(LitMap.find v0 !dict_list)) !dict_list
       
  let to_list dict_list =
    List.flatten (List.map snd (LitMap.bindings !dict_list) )
end
