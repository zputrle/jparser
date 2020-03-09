(*
 * Here you can find a simple JSON parser build with a technique called
 * 'combinatory parsing'.
 *
 * First part of the file contains building blocks that can be used to build
 * parsers in general. Where following is a small JSON parser build upon those
 * blocks with a minimal support for error reporting.
 *
 * This is my attempt to work through the following [1] article written by Ken
 * Lamug.
 * 
 * [1] https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/
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

(* Run the parser and print error message to stdout if an error occurs. *)
let run_and_print_error_msg parser input = 
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

(*
 * JSON parser 
 *)

(*
 * Exceptions
 *)

exception InvalidToken of string

exception UnexpectedError of string

(*
 * Helper functions.
 *)

let explode str = List.init (String.length str) (String.get str)

let implode chrs = String.init (List.length chrs) (List.nth chrs)

let to_string ch = String.make 1 ch

let unwrap ol =
  match ol with
  | Some l -> l
  | None -> []

let to_list oe =
  match oe with
  | Some e -> [e]
  | None -> []

(*
 * Internal representation of the JSON structure.
 *)

type json_value =
  | JNull
  | JTrue
  | JFalse
  | JString of string
  | JNumber of float
  | JArray of json_value list
  | JObject of (string * json_value) list

(* Space parsers. *)

let p_space =
  let is_space = String.contains " \n\t\r"
  in
    (next_char |> satisfy) is_space "whitespace chracter"

let p_spaces =
  one_or_more p_space

let p_spaces_if_available =
  zero_or_more p_space

(* String parsers. *)

let p_char ch =
  (next_char |> satisfy) (fun ich -> ich == ch) ("char '" ^ to_string ch ^ "'")

let p_string =
  let is_alpha = String.contains "_abcdefghijklmnopqrstvwuxyz"
  in
    (next_char |> satisfy) is_alpha "string" |> one_or_more |>> implode <?> "string"

let p_string_exactly str =
  explode str |> List.map p_char |> exactly |>> implode

(* Number parsers. *)

let p_digit_from_1_to_9 =
  let is_digit = String.contains "123456789"
  in
    (next_char |> satisfy) is_digit "digit"

let p_digit =
  let is_digit = String.contains "0123456789"
  in
    (next_char |> satisfy) is_digit "digit"

(* Sequence parser. *)

let p_sequence parser delimiter =
  parser
  >> zero_or_more
    (p_spaces_if_available
     />> (p_char delimiter)
     />> p_spaces_if_available
     />> parser)
  |>> (fun (hd, tl) -> hd :: tl)
  <?> "sequence of " ^ parser.label ^ "s"

(* Parser JSON values. *)

let p_json_null =
  (p_string_exactly "null" |>> fun _ -> JNull) <?> "JSON null value"

let p_json_true =
  (p_string_exactly "true" |>> fun _ -> JTrue) <?> "JSON true value"

let p_json_false =
  (p_string_exactly "false" |>> fun _ -> JFalse) <?> "JSON false value"

let p_json_number =

  let p_prefix = p_char '-' in

  let p_whole_number = 
      let p_zero = (p_char '0') |>> (fun i -> [i])
      and p_seq_of_digits = 
        p_digit_from_1_to_9
        >> zero_or_more p_digit
        |>> fun (hd,tl) -> hd::tl
      in
        p_zero <||> p_seq_of_digits in

  let p_decimal =
      p_char '.'
      >> one_or_more p_digit
      |>> fun (hd,tl) -> hd::tl in

  let p_exponent =
    (p_char 'e' <||> p_char 'E')
    >> optional (p_char '-' <||> p_char '+')
    >> p_whole_number
    |>> fun p ->
      match p with
      | ((_, None), tl) -> 'e'::tl
      | ((_, Some '+'), tl) -> 'e'::tl
      | ((_, Some '-'), tl) -> 'e'::'-'::tl
      | ((_, Some ch), _) ->
        raise (InvalidToken
          (Printf.sprintf "Character not expected. char = '%c'" ch)) in

  let to_float (((p, wn), d), e) =
    float_of_string (implode (p @ wn @ d @ e)) in

    ((optional p_prefix) |>> to_list)
    >> p_whole_number
    >> (optional p_decimal |>> unwrap)
    >> (optional p_exponent |>> unwrap)
    |>> to_float |>> (fun v -> JNumber v)
    <?> "JSON number"

(* Parse JSON string. *)
let _p_json_string =
  let p_unescaped_char = (next_char |> satisfy)
    (fun ch -> not (ch == '\\' || ch == '\"')) (* Must not be '\' or '"'. *)
    "char"
  and p_escaped_char = 
    [("\\\"", '"'); (* quite *)
     ("\\\\", '\\'); (* back slash *)
     ("\\/", '/'); (* slash *)
     ("\\b", '\b'); (* backspace *)
     ("\\f", '\012'); (*formfeed*)
     ("\\n", '\n'); (* newline *)
     ("\\r", '\r'); (* carriage return *)
     ("\\t", '\t')] (* tab *)
    |> (List.map (fun (i, o) -> p_string_exactly i |>> fun _ -> o))
    |> one_of
    <?> "escape char"
  in
    let p_jchar = p_unescaped_char <||> p_escaped_char
    in
      p_char '"' />> (zero_or_more p_jchar) >>/ p_char '"'
      |>> implode
      <?> "JSON string"

let p_json_string =
    _p_json_string
    |>> (fun s -> JString s)

let p_json_object =

  let json_value_label = "JSON value"
  and json_array_label = "JSON array"
  and json_object_label = "JSON object" in

  let rec _p_json_value input =
    let p_json_array = to_parser _p_json_array json_array_label
    and p_json_object = to_parser _p_json_object json_object_label
    in
      let p_json_value = one_of [
        p_json_null;
        p_json_true;
        p_json_false;
        p_json_number;
        p_json_string;
        p_json_array;
        p_json_object]
      in
        run p_json_value input

  and _p_json_array input =
    let p_json_value = to_parser _p_json_value json_value_label
    in
      let p_json_array =
        p_char '['
        />> p_spaces_if_available
        />> (p_sequence p_json_value ',')
        >>/ p_spaces_if_available
        >>/ p_char ']'
        >>/ p_spaces_if_available
        |>> fun a -> JArray a
    in
      run p_json_array input

  and _p_json_object input =
    let p_json_value = to_parser _p_json_value json_value_label
    in
      let p_json_name_value_pair =
        _p_json_string
        >>/ p_spaces_if_available
        >>/ p_char ':'
        >>/ p_spaces_if_available
        >> p_json_value
      in
        let p_json_object =
          p_spaces_if_available
          />> p_char '{'
          />> p_spaces_if_available
          />> optional (
            p_json_name_value_pair
            >> zero_or_more (
              p_spaces_if_available
              />> p_char ','
              />> p_spaces_if_available
              />> p_json_name_value_pair)
              |>> fun (hd, tl) -> hd::tl)
          >>/ p_spaces_if_available
          >>/ p_char '}'
          |>> (fun p -> 
            match p with 
            | None -> JObject []
            | Some s -> JObject s)
          <?> "json object"
        in
          run p_json_object input
  in
    to_parser _p_json_object "JSON object"


exception ParsingError of parser_label * parser_error * parser_position

(* Parse a JSON object. *)
let parse_json_object str =
    match run p_json_object (to_input_state str) with
    | Success (rs, input) -> rs
    | Error (pl, pe, pp) -> raise (ParsingError (pl, pe, pp))