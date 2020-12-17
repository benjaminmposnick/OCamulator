open Ast 

(* Define styles for command-line output *)
let yellow = ANSITerminal.yellow
let red = ANSITerminal.red
let blue = ANSITerminal.blue
let green = ANSITerminal.green
let magenta = ANSITerminal.magenta
let white = ANSITerminal.white
let underline = ANSITerminal.Underlined

(** [cprint styles str] prints [str] to the terminal using [styles] to format
    and color the output. *)
let cprint styles str =
  ANSITerminal.print_string styles str

(** [cprint_newline styles str] prints [str] followed by a newline character to
    the terminal using [styles] to format and color the output. *)
let cprint_newline styles str =
  cprint styles (str ^ "\n")

(** [print_store sigma] prints the contents of store [sigma]. *)
let print_store sigma =
  let rec print_store_aux = function
    | [] -> ()
    | (x, v)::t ->
      cprint [blue] (x ^ " -> ");
      cprint_newline [magenta] (string_of_value v);
      print_store_aux t
  in
  print_store_aux (List.sort compare sigma)

(** [handle_syntax_error lexbuf input] determines the token in [input] where
    the syntax error occurred during lexing and prints a helpful error message
    using the information in [lexbuf] to the terminal indicating the location
    of the error. *)
let handle_syntax_error lexbuf input =
  let pos = lexbuf.Lexing.lex_curr_p in
  let invalid_token_idx = ref (pos.Lexing.pos_cnum - 1) in
  let invalid_token =
    try Char.escaped (input.[!invalid_token_idx]) with 
    (* Token was not even recognized by lexer *)
    | Invalid_argument _ -> failwith "Handled by lexer"
    (* invalid_token_idx := !invalid_token_idx - 1; Char.escaped (input.[!invalid_token_idx]) *)
  in
  let valid_tokens = 
    try String.sub input 0 !invalid_token_idx with
    | Invalid_argument _ -> "" in 
  let unlexed_tokens =
    let num_tokens = (String.length input - !invalid_token_idx - 1) in
    try String.sub input (!invalid_token_idx + 1) num_tokens with
    | Invalid_argument _ -> "" in
  cprint_newline [red] ("Syntax Error: Illegal syntax starting at character "
                        ^ (string_of_int !invalid_token_idx) ^ " on token \""
                        ^ invalid_token ^ "\"");
  cprint [yellow] valid_tokens;
  cprint [red; underline] invalid_token;
  cprint_newline [yellow] unlexed_tokens

(** [event_loop sigma] runs the calculator session, where [sigma] is the
    current store, which maps variable names to their current value in this
    session. *)
let rec event_loop sigma = 
  cprint [white] ">> ";
  let input = read_line () in begin
    if String.lowercase_ascii input = "#quit" then 
      cprint_newline [blue] "Goodbye! Peace, love, and 3110."
    else if String.lowercase_ascii input = "#state" then 
      (print_store sigma; event_loop sigma)
    else
      let lexbuf = Lexing.from_string input in
      let parsed_input = try Some (Parser.prog Lexer.read lexbuf) with
        | _ -> try handle_syntax_error lexbuf input; None with | _ -> None; in
      match parsed_input with
      | None -> event_loop sigma
      | Some ast -> 
        let result = try Some (Eval.eval_input ast sigma) with
          | Eval.ComputationError.EvalError msg -> cprint_newline [red] msg; None
          | Eval.ComputationError.TypeError msg -> cprint_newline [red] msg; None 
          | Failure msg -> cprint_newline [red] msg; None in
        match result with
        | None -> event_loop sigma
        | Some (value, sigma') -> let value_str = string_of_value value in
          cprint_newline [green] ("==> " ^ (value_str)); event_loop sigma'
  end;
  exit 0

let main () =
  cprint_newline [blue] "Welcome to the OCamulator!";
  event_loop [("ans", VFloat 0.)]

let () = main ()