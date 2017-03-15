open Printf;;

let gen_parity n =
  let channel = open_out "parity.form" in
  let printf s = fprintf channel (format_of_string s) in
  let rec aux = function
    | i when i=0 -> ()
    | i -> if i != n then printf " X %d" i else printf "%d" i; aux (i-1)
  in
  aux n;
  printf " 0\n";
  flush channel;
  close_out channel;;
