open Ast 
open Eval

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** Prints [string] (with '\n') to the terminal styled with [color]. *)
let print_in_color color string =
  string ^ "\n" |> ANSITerminal.print_string [color]

let rec event_loop () = 
  print_string ">> ";
  let input = read_line () in
  let ast = parse input in
  string_of_expr ast |> print_in_color ANSITerminal.magenta; 
  let result = eval ast in
  string_of_float result |> print_in_color ANSITerminal.green; event_loop ()

let main () =
  print_in_color ANSITerminal.blue "Welcome to the OCamulator!";
  event_loop ()

let () = main ()