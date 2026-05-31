open Edn_ocaml

let failf fmt = Printf.ksprintf failwith fmt

let ia = Iarray.of_list

let rec pp_value = function
  | Nil -> "Nil"
  | Bool value -> Printf.sprintf "Bool %b" value
  | String value -> Printf.sprintf "String %S" value
  | Char value -> Printf.sprintf "Char U+%04X" (Uchar.to_int value)
  | Symbol value -> Printf.sprintf "Symbol %S" value
  | Keyword value -> Printf.sprintf "Keyword %S" value
  | Int value -> Printf.sprintf "Int %Ld" value
  | Bigint value -> Printf.sprintf "Bigint %S" value
  | Float value -> Printf.sprintf "Float %.17g" value
  | Decimal value -> Printf.sprintf "Decimal %S" value
  | List values ->
      Printf.sprintf "List [%s]" (String.concat "; " (Iarray.to_list (Iarray.map pp_value values)))
  | Vector values ->
      Printf.sprintf "Vector [%s]" (String.concat "; " (Iarray.to_list (Iarray.map pp_value values)))
  | Set values ->
      Printf.sprintf "Set [%s]" (String.concat "; " (Iarray.to_list (Iarray.map pp_value values)))
  | Map entries ->
      let pp_entry (key, value) = Printf.sprintf "%s => %s" (pp_value key) (pp_value value) in
      Printf.sprintf "Map [%s]" (String.concat "; " (Iarray.to_list (Iarray.map pp_entry entries)))
  | Tagged (tag, value) -> Printf.sprintf "Tagged (%S, %s)" tag (pp_value value)

let check_value name expected actual =
  if expected <> actual then failf "%s\nexpected: %s\nactual:   %s" name (pp_value expected) (pp_value actual)

let check_string name expected actual =
  if expected <> actual then failf "%s\nexpected: %S\nactual:   %S" name expected actual

let check_json name expected actual =
  let expected_json = Yojson.Safe.from_string expected in
  let actual_json = Yojson.Safe.from_string actual in
  if expected_json <> actual_json then
    failf "%s\nexpected: %s\nactual:   %s" name
      (Yojson.Safe.pretty_to_string expected_json)
      (Yojson.Safe.pretty_to_string actual_json)

let check_raises name f =
  match f () with
  | exception Parse_error _ -> ()
  | exception Invalid_argument _ -> ()
  | exception exn -> failf "%s\nunexpected exception: %s" name (Printexc.to_string exn)
  | _ -> failf "%s\nexpected an exception" name

let check_parse_error name expected f =
  match f () with
  | exception Parse_error message -> check_string name expected message
  | exception exn -> failf "%s\nunexpected exception: %s" name (Printexc.to_string exn)
  | _ -> failf "%s\nexpected a Parse_error" name

let test_parse_atoms () =
  check_value "nil" Nil (of_edn_string "nil");
  check_value "true" (Bool true) (of_edn_string "true");
  check_value "false" (Bool false) (of_edn_string "false");
  check_value "string escapes" (String "a\tb\n\"c\"\\")
    (of_edn_string {| "a\tb\n\"c\"\\" |});
  check_value "unicode string escape" (String "snowman: \226\152\131")
    (of_edn_string {| "snowman: \u2603" |});
  check_value "character" (Char (Uchar.of_char 'x')) (of_edn_string {|\x|});
  check_value "named character" (Char (Uchar.of_char ' ')) (of_edn_string {|\space|});
  check_value "unicode character" (Char (Uchar.of_int 0x2603)) (of_edn_string {|\u2603|});
  check_value "symbol" (Symbol "my.ns/name") (of_edn_string "my.ns/name");
  check_value "keyword" (Keyword "my.ns/name") (of_edn_string ":my.ns/name");
  check_value "int" (Int 42L) (of_edn_string "+42");
  check_value "negative zero" (Int 0L) (of_edn_string "-0");
  check_value "big int" (Bigint "123456789012345678901234567890")
    (of_edn_string "123456789012345678901234567890N");
  check_value "float" (Float 6.02e23) (of_edn_string "6.02e23");
  check_value "decimal" (Decimal "1.20") (of_edn_string "1.20M")

let test_parse_collections_comments_and_discard () =
  let source =
    {|
      [a b #_foo 42 ; comments run to the end of the line
       {:a 1, "b" [true nil] :c #{foo \space}}]
    |}
  in
  check_value "collections, comments, commas, discard"
    (Vector
       (ia
          [
         Symbol "a";
         Symbol "b";
         Int 42L;
         Map
           (ia
              [
             (Keyword "a", Int 1L);
             (String "b", Vector (ia [ Bool true; Nil ]));
             (Keyword "c", Set (ia [ Symbol "foo"; Char (Uchar.of_char ' ') ]));
              ]);
          ]))
    (of_edn_string source)

let test_tags_and_streaming () =
  check_value "tagged value"
    (Tagged ("inst", String "1985-04-12T23:20:50.52Z"))
    (of_edn_string {|#inst "1985-04-12T23:20:50.52Z"|});
  check_value "read all values"
    (List (ia [ Int 1L; Int 2L; Keyword "done" ]))
    (List (ia (of_edn_string_all "1 #_ignored 2 :done")))

let test_writer () =
  check_string "write atoms" {|[nil true false "a\nb" \space :k sym 42 1.5 123N 1.20M]|}
    (to_edn_string
       (Vector
          (ia
             [
            Nil;
            Bool true;
            Bool false;
            String "a\nb";
            Char (Uchar.of_char ' ');
            Keyword "k";
            Symbol "sym";
            Int 42L;
            Float 1.5;
            Bigint "123";
            Decimal "1.20";
             ])));
  check_string "write collections" {|{:a 1 "b" [true nil] :s #{x y} :tag #my/app {:ok true}}|}
    (to_edn_string
       (Map
          (ia
             [
            (Keyword "a", Int 1L);
            (String "b", Vector (ia [ Bool true; Nil ]));
            (Keyword "s", Set (ia [ Symbol "x"; Symbol "y" ]));
            (Keyword "tag", Tagged ("my/app", Map (ia [ (Keyword "ok", Bool true) ])));
             ])))

let test_json_conversion () =
  let edn =
    Map
      (ia
         [
        (String "name", String "Ada");
        (String "age", Int 37L);
        (String "admin", Bool false);
        (String "tags", Vector (ia [ String "ocaml"; Nil ]));
         ])
  in
  check_value "read json string" edn
    (of_json_string {|{"name":"Ada","age":37,"admin":false,"tags":["ocaml",null]}|});
  check_json "write json string" {|{"name":"Ada","age":37,"admin":false,"tags":["ocaml",null]}|}
    (to_json_string edn);
  check_json "keyword map keys become json object names" {|{"ok":true}|}
    (to_json_string (Map (ia [ (Keyword "ok", Bool true) ])))

let test_errors () =
  check_raises "reject trailing forms" (fun () -> ignore (of_edn_string "1 2"));
  check_raises "reject odd map entry count" (fun () -> ignore (of_edn_string "{:a 1 :b}"));
  check_raises "reject mismatched closing delimiter" (fun () -> ignore (of_edn_string "{:a/b [1 2 3])"));
  check_parse_error "parse error includes position" "at position 13: unexpected closing delimiter: )" (fun () ->
      ignore (of_edn_string "{:a/b [1 2 3])"));
  check_raises "reject invalid keyword" (fun () -> ignore (of_edn_string "::bad"));
  check_raises "reject discard without value" (fun () -> ignore (of_edn_string "[1 #_]"));
  check_raises "reject non-string json object keys" (fun () ->
      ignore (to_json_string (Map (ia [ (Vector (ia [ String "not"; String "a"; String "key" ]), Int 1L) ]))))

let tests =
  [
    ("parse atoms", test_parse_atoms);
    ("parse collections, comments, and discard", test_parse_collections_comments_and_discard);
    ("tags and streaming", test_tags_and_streaming);
    ("writer", test_writer);
    ("json conversion", test_json_conversion);
    ("errors", test_errors);
  ]

let () =
  List.iter
    (fun (name, test) ->
      try test () with
      | exn ->
          Printf.eprintf "FAILED: %s\n%s\n" name (Printexc.to_string exn);
          exit 1)
    tests;
  Printf.printf "ok - %d tests\n" (List.length tests)
