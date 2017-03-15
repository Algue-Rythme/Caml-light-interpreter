open Expr

type literal = Var of int;;
type robdd = LeafFalse | LeafTrue | Node of literal * robdd * robdd * propFormula;;

module type Dictionary =
sig
  type dict
  val create : unit -> dict
  val mem : dict -> robdd -> bool
  val find : dict -> robdd -> robdd
  val add : dict -> robdd -> unit
  val to_list : dict -> robdd list
end
