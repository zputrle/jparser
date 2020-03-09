open Jparser
open Utils

let _ = 
  try 
    let o = parse_json_object "{
        \"patient_name\" : \"John Doe\",
        \"patient_id\" : \"231143\",
        \"sex\" : \"M\",
        \"age\" : 31
      }"
    in
      print_endline (pretty_print o);
      exit 0
  with
    ParsingError (pl, pe, pp) ->
      print_endline (construct_error_msg pl pe pp);
      exit 1