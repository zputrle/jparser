(*
  The insiration for this parser is the following article:
    https://fsharpforfunandprofit.com/series/understanding-parser-combinators.html
*)

open Combinators

(* Exceptions *)

exception InvalidToken of string

(* Helper functions. *)

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
  | JNumber of float
  | JArray of json_value list
  | JObject of (json_value * json_value) list

(* TODO: Create a pretty printer for the json_value objects. *)

(* Space parsers. *)

let p_space =
  let is_space = String.contains " \n\t\r"
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
    (next_char |> satisfy) is_alpha "string" |> one_or_more |>> implode <?> "string"

let p_string_exactly str =
  explode str |> List.map p_char |> exactly |>> implode

;;

run p_string (to_input_state "m_var12") ;;

run (p_string_exactly "if") (to_input_state "irs")

;;

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

;;

run (p_sequence (one_or_more p_digit) ',') (to_input_state "12 ,32, 23")
(* run_and_print (p_sequence p_string ',') (to_input_state "\"abc\" , haha") *)

;;

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

;;

run p_json_number (to_input_state "10");;
run p_json_number (to_input_state "12.20");;
run p_json_number (to_input_state "222.1e-3");;

run p_json_number (to_input_state "00.1");;

exit 1

;;

(* Unicode characters are not supported. Only a basic set of pritable ASCII characters is. *)
let p_json_string =
  let p_unescaped_char = (next_char |> satisfy)
    (fun ch ->
      let ich = int_of_char ch in
        not (ich < 32 || 127 < ich) && (* Only basic printable ASCII characters. *)
        not (ch == '\\' || ch == '\"')) (* Must not be '\' or '"'. *)
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
      |>> (fun s -> JString (implode s))
      <?> "JSON string"

let p_json_value =

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
    to_parser _p_json_value "JSON value" 

;;

run p_json_value (to_input_state "[10, 11, 13]");;

run_and_print p_json_value (to_input_state "
  {
    \"x\" : 1
  }
")

;;

run_and_print p_json_value (to_input_state "{}")