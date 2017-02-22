open Expr

module type OBDD =
sig
  type literal
  type robdd
  val create : propFormula -> robdd
end

