open Expr;;
open List;;

let foldOp op prop = fold_left op (hd prop) (tl prop);;
let fOr = foldOp (fun a b -> And(a, b));;
let fAnd = foldOp (fun a b -> Or(a, b));;
let lit l = Literal(l);;

let to_cnf formula =
  let fresh = ref (fold_left max (-1) (literalsList formula)) in
  let addFreshVariable () = incr fresh; !fresh in
  let rec aux = function
    | Const(true) -> let var = addFreshVariable () in (var, lit var)
    | Const(false) -> let var = addFreshVariable () in (var, lit (-var))
    | Literal(x) -> (x, Const(true))
    | Not(a) -> let lA, a' = aux a in (-lA, a')
    | And(a, b) ->
      let lA, a' = aux a in
      let lB, b' = aux b in
      let var = addFreshVariable () in
      (var, fAnd [a'; b'; fOr [lit (-lA); lit (-lB); lit var]; Or(lit (-var), lit lA); Or(lit (-var), lit lB)])
    | Or(a, b) ->
      let lA, a' = aux a in
      let lB, b' = aux b in
      let var = addFreshVariable () in
      (var, fAnd [a'; b'; fOr [lit lA; lit lB; lit (-var)]; Or(lit (-lA), lit var); Or(lit (-lB), lit var)])
    | Xor(a, b) -> aux (And(Or(a, b), Not(And(a, b)))) (* deal with it *)
    | Implies(a, b) -> aux (Or(b, Not(a))) (* flemme *)
    | Equivalent(a, b) -> aux (Not(Xor(a, b))) (* flemme *)
  in
  let l, f = aux formula in
  And(lit l, f)
;;
