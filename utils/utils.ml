open Jparser

(**
 * Pretty print the json structure.
 *
 * TODO:
 * - Cleanup pretty printer.
 * - When printing JNumber, omit the decimal mark.
 *)
let pretty_print jstruct =
  let shift_right n = (String.init (n * 4) (fun _ -> ' ')) in
  let rec _pretty_print depth jstruct =
    match jstruct with
    | JNull -> "null"
    | JTrue -> "true"
    | JFalse -> "false"
    | JString s -> Printf.sprintf "\"%s\"" s
    | JNumber f -> string_of_float f
    | JArray a ->
      "[" ^ (String.concat ", " (List.map (_pretty_print (depth + 1)) a)) ^ "]"
    | JObject o ->
      let sr_braces = shift_right depth
      and sr_values = shift_right (depth + 1)
      in
        "\n" ^ sr_braces ^ "{\n" ^
        sr_values ^
          (String.concat
            (",\n" ^ sr_values)
            (List.map (fun (k,v) ->
              (sr_values ^ k) ^ " : " ^
              (_pretty_print (depth + 1) v))
            o)) ^
        "\n" ^ sr_braces ^ "}"
  in
    _pretty_print 0 jstruct

let to_string rt =
  match rt with
  | Success (rs, inp) -> pretty_print rs
  | Error (label, error, pp) -> construct_error_msg label error pp