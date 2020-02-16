(*
 * This is a library that can be used to build parsers with a technique called
 * combinatory parsing (https://en.wikipedia.org/wiki/Parser_combinator).
 *
 * It provides basic building blocks for creating 'applicative parsers' with a
 * minimal support for error reporting.
 *)

(*
 * Input representation and walk through support
 *)

type position = {
  line: int;
  column: int
}

type input = string list

type input_state = {
  input: input;
  position: position
}

let next_line in_st = {
  input = in_st.input;
  position = {
    line = in_st.position.line + 1;
    column = 0
  }
}

let next_column in_st = {
  input = in_st.input;
  position = {
    line = in_st.position.line;
    column = in_st.position.column + 1
  }
}

let to_input_state str = {
  input = (String.split_on_char '\n' str);
  position = {line = 0; column = 0}
}

let next_char in_st =
  if  in_st.position.line >= (List.length in_st.input) then
    (None, in_st)(* No more characters. *)
  else 
    let current_line = List.nth in_st.input in_st.position.line
    in
      if in_st.position.column >= (String.length current_line) then
        (Some '\n', next_line in_st) (* Go to next line. *)
      else
        let ch = String.get current_line in_st.position.column
        in (Some ch, next_column in_st)

type parser_position = {
  current_line: string option;
  position: position
}

let to_parser_position in_st = {
  current_line = (List.nth_opt in_st.input in_st.position.line);
  position = in_st.position
}

type parser_label = string

type parser_error = string

(*
 * Basic parsers
 *)

(* Pasing result - what parsers return *)
type 'a result =
  | Error of parser_label * parser_error * parser_position
  | Success of 'a * input_state

(* Representation of a parser *)
type 'a parser = {
  p_fun: (input_state -> 'a result);
  label: parser_label
}

let to_parser pf l = {p_fun = pf; label = l} 

(* Apply the parser on the provided input. *)
let run parser input = 
  match parser.p_fun input with
  | Success _ as s -> s
  | Error (_, error_msg, loc) -> Error (parser.label, error_msg, loc)

(* Construct a parser that takes the next element from the input stream and
 * and checks that it satisfies the provided predicate. *)
let satisfy next predicate label =
  let _pred_pars input =
    match (next input) with
    | (None, in_st) ->
        Error ("Expected " ^ label ^ ".", "EOF", to_parser_position in_st)
    | (Some el, in_st) ->
      if predicate el then
        Success (el, in_st)
      else
        Error (label, "Expected " ^ label ^ ".", to_parser_position in_st)
  in
    to_parser _pred_pars label

(* Construct a parser that will always return the provided value. *)
let return v =
  let _return input =
    Success (v, input)
  in
    to_parser _return "return"

(* Construct a parser that will always return an error with the provided
 * error message. *)
let return_error msg =
  let label = "return_error"
  in let _return_error input =
    Error (label, msg, to_parser_position input)
  in
    to_parser _return_error label

(* Apply the first or the second parser *)
let or_else parser1 parser2 =
  let _or_else input =
    match run parser1 input with
    | Error _ -> run parser2 input
    | Success _ as sc -> sc
  in
    to_parser _or_else (parser1.label ^ " or " ^ parser2.label)

let (<||>) = or_else

(* Bind a parser to the function f that accepts the result of the first parser
 * and returns a new parser based on the parsed value. *)
let bind f parser =
  let _bind input =
    match run parser input with
    | Error _ as e -> e
    | Success (hd, tl) -> 
      run (f hd) tl
  in
    to_parser _bind ("bind " ^ parser.label)

let (>>=) parser f = bind f parser

(*
 * Support for better error reporting.
 *)

(* Construct an error message. *)
let construct_error_msg label error_msg pp =
  let line_num = pp.position.line
  and column_num = pp.position.column
  in
    (Printf.sprintf
      "Line:%i Col:%i Error parsing %s\n"
      line_num column_num label) ^
    (* Append additonal information if current line is provided.*)
    match pp.current_line with
      | None -> "" (* Current line is not provided. Nothing to be done.*)
      | Some current_line ->
        let shift_right = (String.init pp.position.column (fun _ -> ' '))
        in
          (Printf.sprintf "%s\n" current_line) ^
          (Printf.sprintf ("%s^ %s\n") shift_right error_msg)

(* Run parser*)
let run_and_print parser input = 
  match run parser input with
  | Success _ as s -> s
  | Error (label, error_msg, pp) as e ->
    let _ = print_string (construct_error_msg parser.label error_msg pp)
    in e

(*
 * Derived parsers
 *)

(* Change the label of the parser. *)
let change_label new_label parser = {p_fun = parser.p_fun; label = new_label}

let (<?>) parser new_label = change_label new_label parser

(* Apply the first and then the second parser if the first one was applied
 * successfully. Otherwise, return an error. If both parsers are successfully
 * applied, return parsed values as a tuple.
 *)
let and_then parser1 parser2 =
  parser1 >>= fun hd1 -> parser2 >>= fun hd2 -> return (hd1, hd2)

let (>>) = and_then

(* Apply the first and then the second parser if the first one was applied
 * successfully. Otherwise, return an error. If both parsers are successfully
 * applied, return only the value parsed by the second parser.
 *)
let (/>>) parser1 parser2 =
  parser1 >>= fun _ -> parser2

(* Apply the first and then the second parser if the first one was applied
 * successfully. Otherwise, return an error. If both parsers are successfully
 * applied, return only the value parsed by the first parser.
 *)
let (>>/) parser1 parser2 =
  parser1 >>= fun hd -> (parser2 />> return hd)

(* Apply all the parsers. If all are applied successfully, return parsed values.
 * Otherwise, return an error. *)
let exactly parsers =
  let _exactly input =
    let rec r_exactly ps inp rs =
      match ps with
      | [] -> Success (rs, inp)
      | p::lp ->
        match run p inp with
        | Error _ as e -> e
        | Success (hd2, tl2) ->
          r_exactly lp tl2 (rs @ hd2::[])
    in
    r_exactly parsers input []
  in
    to_parser _exactly "exactly"

(* Apply the parser and map the resulting value. *)
let map f parser =
  parser >>= fun hd -> return (f hd)

let (|>>) parser f = map f parser

(* Apply one of the parsers. If non is applied successfully, return an error. *)
let one_of parsers =
  List.fold_right ( <||> ) parsers
    (return_error "None of the parsers could be applied.")

(* Apply parser zero or more times. *)
let zero_or_more parser =
  let rec _zero_or_more input =
    match run parser input with
    | Error _ -> Success ([] , input)
    | Success (hd1, tl1) ->
      match _zero_or_more tl1 with
      | Error _ as er -> er
      | Success (hd2, tl2) -> Success (hd1::hd2, tl2)
  in
    to_parser _zero_or_more ("zero or more " ^ parser.label)

(* Apply parser one or more times. *)
let one_or_more parser =
  let rec _one_or_more input =
    match run parser input with
    | Error _ as e -> e
    | Success (hd1, tl1) ->
      match run (zero_or_more parser) tl1 with
        | Error _ as e -> e
        | Success (hd2, tl2) -> Success (hd1::hd2, tl2)
  in
    to_parser _one_or_more ("one or more " ^ parser.label)

(* Apply a parser if possible. *)
let optional parser =
  let _parser = parser |>> fun s -> Some s
  and _none = return None
  in
    _parser <||> _none