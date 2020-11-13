open Ast 
open Eval

(* Define colors for command-line output *)
let yellow = ANSITerminal.yellow
let red = ANSITerminal.red
let blue = ANSITerminal.blue
let green = ANSITerminal.green
let magenta = ANSITerminal.magenta

(** [print_in_color color str] prints [str] followed by a newline character to
    the terminal styled with [color]. *)
let print_in_color color str =
  str ^ "\n" |> ANSITerminal.print_string [color]

(** [print_store sigma] prints the contents of store [sigma] *)
let print_store sigma =
  let rec print_store_aux = function
    | [] -> ()
    | (x, v)::t ->
      ANSITerminal.print_string [blue] (x ^ " -> ");
      print_in_color magenta (string_of_expr v);
      print_store_aux t
  in
  if List.length sigma = 0 then print_in_color green "No variables in scope"
  else print_store_aux (List.sort compare sigma)

(** [event_loop sigma ans] runs the calculator session, where [sigma] is the
    current store, which maps variable names to their current value in the
    session, and [ans] is the result of the last computation. *)
let rec event_loop sigma ans = 
  print_string ">> ";
  let input = read_line () in
  let () =
    if String.lowercase_ascii input = "quit" then 
      print_in_color blue "Goodbye! Peace, love, and 3110."
    else if String.lowercase_ascii input = "scope" then 
      (print_store sigma; event_loop sigma ans)
    else
      let (ans, sigma') =
        try
          let parsed_input = parse input in
          string_of_input parsed_input
          |> ( ^ ) "Parsed input: " 
          |> print_in_color yellow;
          let (result, sigma') = eval_input parsed_input sigma ans in
          string_of_expr result
          |> ( ^ )  "==> "
          |> print_in_color green;
          (result, sigma')
        with 
        | Parser.Error -> print_in_color red "Invalid input"; (ans, sigma)
        | Not_found -> print_in_color red "Variable is not in scope"; (ans, sigma)
        | e -> print_in_color red "An error occurred during evaluation"; (ans, sigma)
      in
      event_loop sigma' ans
  in
  exit 0

let main () =
  print_in_color ANSITerminal.blue "Welcome to the OCamulator!";
  event_loop [] (Float 0.)

let () = main ()