open Expr;;
open List;;

type cnf_literal = CNF_Literal of int;;
type clause = Clause of cnf_literal list;;
type cnf = CNF of clause list;;

let concat_and cnfs = CNF(concat (map (function CNF(l) -> l) cnfs));;
let concat_or clauses = Clause(concat (map (function Clause(l) -> l) clauses));;
let lit l = CNF_Literal(l);;
let fOr l = CNF([Clause(map (function i -> lit i) l)]);;
let fAnd = concat_and;;

let to_cnf formula =
  let fresh = ref (fold_left max (-1) (literalsList formula)) in
  let addFreshVariable () = incr fresh; !fresh in
  let rec aux = function
    | Const(true) -> let var = addFreshVariable () in (var, CNF([Clause([lit var])]))
    | Const(false) -> let var = addFreshVariable () in (var, CNF([Clause([lit (-var)])]))
    | Literal(x) -> (x, CNF([]))
    | Not(a) -> let lA, a' = aux a in (-lA, a')
    | And(a, b) ->
      let lA, a' = aux a in
      let lB, b' = aux b in
      let var = addFreshVariable () in
      (var, fAnd [a'; b'; fOr [-lA; -lB; var]; fOr [-var; lA]; fOr [-var; lB]])
    | Or(a, b) ->
      let lA, a' = aux a in
      let lB, b' = aux b in
      let var = addFreshVariable () in
      (var, fAnd [a'; b'; fOr [lA; lB; -var]; fOr [-lA; var]; fOr [-lB; var]])
    | Xor(a, b) -> aux (And(Or(a, b), Not(And(a, b)))) (* deal with it *)
    | Implies(a, b) -> aux (Or(b, Not(a))) (* flemme *)
    | Equivalent(a, b) -> aux (Not(Xor(a, b))) (* flemme *)
  in
  let l, f = aux formula in
  fAnd [fOr [l]; f];;