open Sifting_utils;;
open Expr;;
open Dictionary;;
open Dict_litHash;;

(* swap i and i+1 *)
let swap sift i =
  let vi = Var(i) and vip1 = Var(i+1) in
  let listeNoeuds = LitHash.find sift.lvlTable vi in
  ()
