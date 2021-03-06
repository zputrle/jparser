open OUnit2

open Jparser

exception ExpectedSuccess of parser_label * parser_error * parser_position
exception ExpectedError of json_value * input_state

(* Helper functions *)

let return_jobject rt =
  match rt with
  | Success (rs, inp) -> rs
  | Error (pl, pe, pp) -> raise (ExpectedSuccess (pl, pe, pp))

let return_error rt =
  match rt with
  | Success (rs, inp) -> raise (ExpectedError (rs, inp))
  | Error (pl, pe, pp) -> (pl, pe, pp)

let parse_with parser str =
  return_jobject (run parser (to_input_state str))

let parse_with_and_return_error parser str =
  return_error (run parser (to_input_state str))

let parse str =
  parse_json_object str

(* Tests *)

let test_space_parsing _ =
  (* Test if how spaces are parsed. *)

  assert_equal
    "   "
    (implode (parse_with p_spaces "   123"));

  assert_equal
    ""
    (implode (parse_with p_spaces_if_available "123"))

let test_string_parsing _ =

  assert_equal
    "if"
    (parse_with (p_string_exactly "if") "if") 

let test_sequence_parsing _ =

  assert_equal
    ['1'; '2'; '3']
    (parse_with (p_sequence p_digit ',') "1 , 2, 3")

let test_decimal_parsing _ =

  assert_equal
    (JNumber 10.)
    (parse_with p_json_number "10");

  assert_equal
    (JNumber 12.2)
    (parse_with p_json_number "12.20");

  assert_equal
    (JNumber 0.2221)
    (parse_with p_json_number "222.1e-3")

let test_JSON_string_parsing _ =
    (* Test if a JSON string is parsed. *)

    assert_equal
      (JString "")
      (parse_with p_json_string "\"\"");

    assert_equal
      (JString "abc")
      (parse_with p_json_string "\"abc\"");

    let (pl, pe, pp) = (parse_with_and_return_error p_json_string "\"a\\yc\"")
    in
      assert_equal "Expected char '\"'." pe;

    assert_equal
      (JString "\\u0123")
      (parse_with p_json_string "\"\\u0123\"");

    assert_equal
      (JString "a\\u0123")
      (parse_with p_json_string "\"a\\u0123\"");

    assert_equal
      (JString "a\\u0123aba")
      (parse_with p_json_string "\"a\\u0123aba\"");

    assert_equal
      (JString "a\\u0123\\uFFF0aba")
      (parse_with p_json_string "\"a\\u0123\\uFFF0aba\"")

let test_empty_object_parsing _ = 
    (* Test if an empty object is parsed. *)

    assert_equal
      (JObject [])
      (parse "{}")

let test_simple_object_parsing _ = 
    (* Test if a simple JSON object is parsed. *)

    assert_equal

      (JObject [
        ("x", JNumber 1.);
        ("y", JNumber 2.1)])

      (parse "
          {
            \"x\" : 1,
            \"y\" : 2.1
          }
        ")

let test_nested_object_parsing _ = 
  (* Test if a nested object is parsed. *)

    assert_equal

      (JObject [
        ("x", JNumber 1.);
        ("v", JObject [
            ("y-axis", JNumber 739.);
            ("z-axis", JNumber 3.14)
          ])
        ])

      (parse "
          {
            \"x\" : 1,
            \"v\" : {
              \"y-axis\" : 739,
              \"z-axis\" : 3.14
            }
          }
        ")

let test_object_parsing_example_1 _ = 
  (* Test if a nested object is parsed. *)

  assert_equal

    (JObject [
      ("glossary", JObject [
        ("title", JString "example glossary");
        ("GlossDiv", JObject [
          ("title", JString "S");
          ("GlossList", JObject [
            ("GlossEntry", JObject [
              ("ID", JString "SGML");
              ("SortAs", JString "SGML");
              ("GlossTerm", JString "Standard Generalized Markup Language");
              ("Acronym", JString "SGML");
              ("Abbrev", JString "ISO 8879:1986");
              ("GlossDef", JObject [
                  ("para", JString "A meta-markup language, used to create markup languages such as DocBook.");
                  ("GlossSeeAlso",
                    JArray [JString "GML"; JString "XML"])
                ]);
              ("GlossSee", JString "markup")
            ])
          ])
        ])
      ])
    ])

    (parse "
      {
        \"glossary\": {
          \"title\": \"example glossary\",
          \"GlossDiv\": {
            \"title\": \"S\",
            \"GlossList\": {
              \"GlossEntry\": {
                \"ID\": \"SGML\",
                \"SortAs\": \"SGML\",
                \"GlossTerm\": \"Standard Generalized Markup Language\",
                \"Acronym\": \"SGML\",
                \"Abbrev\": \"ISO 8879:1986\",
                \"GlossDef\": {
                  \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",
                  \"GlossSeeAlso\": [\"GML\", \"XML\"]
                },
                \"GlossSee\": \"markup\"
              }
            }
          }
        }
      }
    ")


let suite =
  "JsonParserTests" >::: [
    "test_space_parsing" >:: test_space_parsing;
    "test_string_parsing" >:: test_string_parsing;
    "test_sequence_parsing" >:: test_sequence_parsing;
    "test_decimal_parsing" >:: test_decimal_parsing;
    "test_JSON_string_parsing" >:: test_JSON_string_parsing;
    "test_empty_object_parsing" >:: test_empty_object_parsing;
    "test_simple_object_parsing" >:: test_simple_object_parsing;
    "test_nested_object_parsing" >:: test_nested_object_parsing;
    "test_object_parsing_example_1" >:: test_object_parsing_example_1
  ]

let () =
  run_test_tt_main suite
