open Ast 
open Eval

(* Define colors *)
let yellow = ANSITerminal.yellow
let red = ANSITerminal.red
let blue = ANSITerminal.blue
let green = ANSITerminal.green

(** [print_in_color color str] prints [str] and a newline to the terminal styled
    with [color]. *)
let print_in_color color str =
  str ^ "\n" |> ANSITerminal.print_string [color]

(** [event_loop ()] runs the calculator session *)
let rec event_loop () = 
  print_string ">> ";
  let input = read_line () in
  let _ = 
    if String.lowercase_ascii input = "quit" then 
      print_in_color blue "Goodbye! Peace, love, and 3110."
    else 
      let _ =
        try 
          let parsed_input = parse input in
          string_of_input parsed_input |> ( ^ ) "Parsed input: " |> print_in_color yellow;
          let result = eval parsed_input in
          string_of_expr result |> ( ^ )  "==> " |> print_in_color green;
        with
        | Parser.Error -> print_in_color red "Invalid input"
        | _ -> print_in_color red "An error occurred during evaluation" in
      event_loop () in
  exit 0

let main () =
  print_in_color ANSITerminal.blue "Welcome to the OCamulator!";
  event_loop ()

let () = main ()