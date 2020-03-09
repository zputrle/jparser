open OUnit2

open Jparser

(* Helper functions *)

(* Return parsed object. Otherwise, raise an exception.*)
let return_jobject rt =
  match rt with
  | Success (rs, inp) -> rs
  | Error (pl, pe, pp) -> raise (ParsingError (pl, pe, pp))

let parse_with parser str =
  return_jobject (run parser (to_input_state str))

(* Parse a JSON object.
 *
 * Return the parsed JObject if successful. Otherwise, raise an exception.
 *)
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
    ["ar"; "ha"; "ls"]
    (parse_with (p_sequence p_string ',') "ar , ha, ls")

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
    "test_empty_object_parsing" >:: test_empty_object_parsing;
    "test_simple_object_parsing" >:: test_simple_object_parsing;
    "test_nested_object_parsing" >:: test_nested_object_parsing;
    "test_object_parsing_example_1" >:: test_object_parsing_example_1
  ]

let () =
  run_test_tt_main suite
