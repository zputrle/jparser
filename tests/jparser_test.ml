open OUnit2

open Jparser

(* Helper functions *)

exception UnexpectedError of string

let return_jobject rt =
  (* Return parsed object. Otherwise, raise an exception.*)

  match rt with
  | Success (rs, inp) -> rs
  | Error (label, error, pp) ->
    raise (UnexpectedError (construct_error_msg label error pp))

(* Tests *)

let test_simple_json_object_parsing _ = 
    (* Parser a simple JSON object test*)

    assert_equal

      (JObject [
        (JString "x", JNumber 1.);
        (JString "y", JNumber 2.1)])

      (return_jobject
        (run p_json_object (to_input_state "
          {
            \"x\" : 1,
            \"y\" : 2.1
          }
        ")))



let suite =
  "JsonParserTests" >::: [
    "test_simple_json_object_parsing" >:: test_simple_json_object_parsing
  ]

let () =
  run_test_tt_main suite
