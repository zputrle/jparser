(*
  The insiration for this parser is the following article:
    https://fsharpforfunandprofit.com/series/understanding-parser-combinators.html
*)

(* Helper functions. *)

let explode str = List.init (String.length str) (String.get str)

let implode chrs = String.init (List.length chrs) (List.nth chrs)

let to_string ch = String.make 1 ch

(* Support for better error reporting. *)

type position = {
  line: int;
  column: int
}

type input_state = {
  input: string list;
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

let to_input_state input_string = {
  input = (String.split_on_char '\n' input_string);
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

(* Basic parsers. *)

type 'a result =
  | Error of parser_label * parser_error * parser_position
  | Success of 'a * input_state

type 'a parser = {
  p_fun: (input_state -> 'a result);
  label: parser_label
}

let to_parser pf l = {p_fun = pf; label = l} 

let run parser input = 
  match parser.p_fun input with
  | Success _ as s -> s
  | Error (_, error_msg, loc) -> Error (parser.label, error_msg, loc)

let return a =
  let _return input =
    Success (a, input)
  in
    to_parser _return "return"

let return_error msg =
  let label = "return_error"
  in let _return_error input =
    Error (label, msg, to_parser_position input)
  in
    to_parser _return_error label

let or_else parser1 parser2 =
  let _or_else input =
    match run parser1 input with
    | Error _ -> run parser2 input
    | Success _ as sc -> sc
  in
    to_parser _or_else (parser1.label ^ " or " ^ parser2.label)

let (||) = or_else

let bind f parser =
  let _bind input =
    match run parser input with
    | Error _ as e -> e
    | Success (hd, tl) -> 
      run (f hd) tl
  in
    to_parser _bind ("bind " ^ parser.label)

let (>>=) parser f = bind f parser

(* Support for better error reporting. *)

let construct_error_msg (Error (label, error_msg, pp)) =
  let line_num = pp.position.line
  and column_num = pp.position.column
  in
    (Printf.sprintf "Line:%i Col:%i Error parsing %s\n" line_num column_num label) ^
      match pp.current_line with
        | None -> "" (* Current line is not provided. Nothing to be done.*)
        | Some current_line ->
          let shift_right = (String.init pp.position.column (fun _ -> ' '))
          in
            (Printf.sprintf "%s\n" current_line) ^
            (Printf.sprintf ("%s^ %s\n") shift_right error_msg)

let run_and_print parser input = 
  match run parser input with
  | Success _ as s -> s
  | Error (label, error_msg, pp) as e ->
  let _ = print_string (construct_error_msg (Error(parser.label, error_msg, pp)))
  in e

(* Derived parsers. *)

let change_label new_label parser = {p_fun = parser.p_fun; label = new_label}

let (^>) parser new_label = change_label new_label parser

let (/>>) parser1 parser2 =
  parser1 >>= fun _ -> parser2

let (>>/) parser1 parser2 =
  parser1 >>= fun hd -> (parser2 />> return hd)

let and_then parser1 parser2 =
  parser1 >>= fun hd1 -> parser2 >>= fun hd2 -> return (hd1, hd2)

let (>>) = and_then

let exactly lparsers =
  let _exactly input =
    let rec __exactly _lp _inp _rs =
      match _lp with
      | [] -> Success (_rs, _inp)
      | p::lp ->
        match run p _inp with
        | Error _ as e -> e
        | Success (hd2, tl2) ->
          __exactly lp tl2 (_rs @ hd2::[])
    in
    __exactly lparsers input []
  in
    to_parser _exactly "exactly"

let map f parser =
  parser >>= fun hd -> return (f hd)

let (|>>) parser f = map f parser

(* let one_of parsers = List.fold_right ( || ) parsers (return_error "None of the parsers could be applied.") *)

let one_of parsers =
  let rec _one_of _parsers input =
    match _parsers with
    | [] -> Error ("one_of", "None of the parsers could be applied.", to_parser_position input)
    | p::ps -> 
      match run p input with
      | Success _ as s -> s
      | Error _ ->
        (_one_of ps input)
  in
    to_parser (_one_of parsers) "one_of"

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

(*
  JSON parser.

  Additional resources:
  - JSON convention explained: https://www.json.org/json-en.html
  - Writing a JSON parser from scratch: https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-4/
*)

(* Internal representation of the JSON structure. *)
type json_value =
  | JNull
  | JTrue
  | JFalse
  | JString of string
  | JNumber of int
  | JArray of json_value list
  | JObject of (json_value * json_value) list

(* TODO: Create a pretty printer for the json_value objects. *)

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

(* Space parsers. *)

let p_space =
  let
    is_space = String.contains " \n\t\r"
  in
    (next_char |> satisfy) is_space "whitespace chracter"

let p_spaces =
  one_or_more p_space

let p_spaces_if_available =
  zero_or_more p_space

;;

run p_spaces (to_input_state "   123")

;;

(* String parsers. *)

let p_char ch =
  (next_char |> satisfy) (fun ich -> ich == ch) ("char '" ^ to_string ch ^ "'")

let p_string =
  let is_alpha = String.contains "_abcdefghijklmnopqrstvwuxyz"
  in
    ((next_char |> satisfy) is_alpha "string" |> one_or_more |>> implode) ^> "string"

let p_string_exactly str =
  explode str |> List.map p_char |> exactly |>> implode

;;

run p_string (to_input_state "m_var12") ;;

run (p_string_exactly "if") (to_input_state "irs")

;;

(* Number parsers. *)

let parse_digit =
  let is_digit = String.contains "0123456789"
  in
    (next_char |> satisfy) is_digit "digit"

let p_number =
  (one_or_more parse_digit)
  |>> implode
  |>> int_of_string

let p_number_if_avaiable =
  (zero_or_more parse_digit)
  |>> implode
  >>= 
    fun hd -> 
      if (String.length hd) = 0
      then return []
      else return ((int_of_string hd) :: [])
let p_space_sep_nums = 
  one_or_more (p_number >>/ p_spaces_if_available)

;;

run p_number_if_avaiable (to_input_state "123A");;

run p_space_sep_nums (to_input_state "12 34 56")

;;

(* Sequence parser. *)

let p_sequence parser delimiter =
  let _ = print_endline "in p_sequence"
  in
  (parser
  >> zero_or_more
    (p_spaces_if_available
     />> (p_char delimiter)
     />> p_spaces_if_available
     />> parser)
  |>> (fun (hd, tl) -> hd :: tl))
  ^> "sequence of " ^ parser.label ^ "s"

;;

run (p_sequence p_number ',') (to_input_state "12 ,32, 23");;
run_and_print (p_sequence p_string ',') (to_input_state "\"abc\" , haha")

;;

(* Parser JSON values. *)

let p_json_null =
  (p_string_exactly "null" |>> fun _ -> JNull) ^> "JSON null value"

let p_json_true =
  (p_string_exactly "true" |>> fun _ -> JTrue) ^> "JSON true value"

let p_json_false =
  (p_string_exactly "false" |>> fun _ -> JFalse) ^> "JSON false value"

let p_json_number =
  (p_number |>> (fun i -> JNumber i)) ^> "JSON number"

let p_json_string =
  (p_char '"' />> p_string >>/ p_char '"' |>> fun s -> JString s) ^> "JSON string"

;;

List.hd
    [p_json_null;
     p_json_true;
     p_json_false;
     p_json_number]

;;

let p_json_value = 
  let rec _p_json_value input =
    let p_json_array = to_parser _p_json_array "JSON array"
    and p_json_object = to_parser _p_json_object "JSON object"
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
    let p_json_value = to_parser _p_json_value "JSON value"
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
  let p_json_value = to_parser _p_json_value "JSON value"
  in
    let p_json_name_value_pair =
      p_json_string
      >>/ p_spaces_if_available
      >>/ p_char ':'
      >>/ p_spaces_if_available
      >> p_json_value
    in
      let p_json_object =
        p_spaces_if_available
        />> p_char '{'
        />> p_spaces_if_available
        />> p_json_name_value_pair
        >> zero_or_more
          (p_spaces_if_available
          />> p_char ','
          />> p_spaces_if_available
          />> p_json_name_value_pair)
        >>/ p_spaces_if_available
        >>/ p_char '}'
        |>> (fun (hd, tl) -> hd :: tl)
        |> change_label "json object"
        |>> fun p -> JObject p 
      in
        run p_json_object input
in
  to_parser _p_json_value "JSON value" 

;;

run p_json_value (to_input_state "[10, 11, 13]");;

run_and_print p_json_value (to_input_state "
  {
    \"x\" : 1,
    \"y\" : \"deset\"
  }
")