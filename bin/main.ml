open Syntax
open Parser

let rec repl () =
  print_string "> ";
  flush stdout;
  let s : string = read_line () in
  Printf.printf "; %s\n\n" (show_exp (parse s));
  repl ()

;;
repl ()
