open Expr

module type OBDD =
sig
  type literal
  type robdd
  val create : propFormula -> robdd
  val to_dot : robdd -> string -> unit (* write the BDD to the file selected in the dot format *) 
end

