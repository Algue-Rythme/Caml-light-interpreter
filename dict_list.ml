open Dictionary;;

module ROBDD_LIST =
struct
  type dict = robdd list ref
  let create () = ref []
  let mem dict_list node = List.mem node (!dict_list)
  let find dict_list node = List.find (fun other -> node = other) (!dict_list)
  let add dict_list node = dict_list := node::(!dict_list)
  let to_list dict_list = !dict_list
end;;
