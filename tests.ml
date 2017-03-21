open Printf;;

(*
utilitity function to simulate imperative loop on an interval
sorry, when I wrote all that I just forgot the imperative part of OCaml
because I was thinking functionnal too often
*)

let apply_range f g a b =
  let rec aux = function
    | i when i = b -> g i
    | i -> f i; aux (i+1)
  in aux a;;

(* Produce a formula satisfiable only with an odd number of literals set to true *)
let gen_parity n =
  let channel = open_out "out/parity.form" in
  let printf s = fprintf channel (format_of_string s) in
  apply_range (fun i -> if i != 1 then printf " X %d" i else printf "%d" i) (fun i -> printf " 0\n") 1 n;
  close_out channel;;

(* something of the form 1 => 2 => 3 => ... => (n-1) => n => 1 *)
let gen_rotations n =
  let channel = open_out "out/rotation.form" in
  let printf s = fprintf channel (format_of_string s) in
  apply_range (fun i -> printf "%d => " i ) (fun i -> printf "%d => %d 0\n" n 1) 1 n;
  close_out channel;;

(* print the formula of pigeonhole principle according to the example given *)
let gen_pigeonhole n =
  let channel = open_out "out/pigeonhole.form" in
  let printf s = fprintf channel (format_of_string s) in
  let var p t = (p-1)*n + t in
  let n_or p = printf "("; apply_range (fun t -> printf "%d \\/ " (var p t)) (fun t -> printf "%d" (var p n)) 1 n; printf ")" in
  apply_range (fun p -> n_or p; printf " /\\ ") (fun p -> n_or (n+1)) 1 (n+1);
  printf " => ";
  let n_dor p q = apply_range (fun t -> printf "(%d /\\ %d) \\/ " (var p t) (var q t)) (fun t -> printf "(%d /\\ %d)" (var p n) (var q n)) 1 n in
  let n_dand q = apply_range (fun p -> n_dor p q; printf " \\/ ") (fun p -> n_dor p (n+1)) 1 (n+1) in
  apply_range (fun q -> n_dand q; printf " \\/ ") (fun p -> n_dand (n+1); printf " 0 \n") 1 (n+1);
  close_out channel;;
