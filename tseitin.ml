open Expr;;
open List;;

type cnf_literal = CNF_Literal of int;;
type clause = Clause of cnf_literal list;;
type cnf = CNF of clause list;;

let dezip_literal = function | CNF_Literal(i) -> i;;
let dezip_clause = function | Clause(clause) -> clause;;
let dezip_cnf = function | CNF(cnf) -> cnf;;

let concat_and cnfs = CNF(concat (map dezip_cnf cnfs));;
let concat_or clauses = Clause(concat (map dezip_clause clauses));;

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
    | Xor(a, b) ->
      let lA, a' = aux a in
      let lB, b' = aux b in
      let var = addFreshVariable () in
      (var, fAnd [a'; b'; fOr [var; -lA; lB]; fOr [var; lA; -lB]; fOr [-var; lA; lB]; fOr [-var; -lA; -lB]])
    | Implies(a, b) ->
      let lA, a' = aux a in
      let lB, b' = aux b in
      let var = addFreshVariable () in
      (var, fAnd [a'; b'; fOr [-var; -lA; lB]; fOr [lA; var]; fOr [-lB; var]])
    | Equivalent(a, b) ->
      let lA, a' = aux a in
      let lB, b' = aux b in
      let var = addFreshVariable () in
      (var, fAnd [a'; b'; fOr [-var; -lA; lB]; fOr [-var; lA; -lB]; fOr [var; lA; lB]; fOr [var; -lA; -lB]])
  in
  let l, f = aux formula in
  fAnd [fOr [l]; f];;
