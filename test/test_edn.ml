open Edn_ocaml

let failf fmt = Printf.ksprintf failwith fmt
let ia = Iarray.of_list
let v value = Any value
let nil = v Nil
let bool value = v (Bool value)
let string value = v (String value)
let char value = v (Char value)
let symbol value = v (Symbol value)
let keyword value = v (Keyword value)
let int value = v (Int value)
let bigint value = v (Bigint value)
let float value = v (Float value)
let decimal value = v (Decimal value)
let list values = v (List (ia values))
let vector values = v (Vector (ia values))
let map entries = v (Map (ia entries))
let set values = v (Set (ia values))
let tagged tag value = v (Tagged (tag, value))

let rec pp_value (Any value) =
  match value with
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
      Printf.sprintf "List [%s]"
        (String.concat "; " (Iarray.to_list (Iarray.map pp_value values)))
  | Vector values ->
      Printf.sprintf "Vector [%s]"
        (String.concat "; " (Iarray.to_list (Iarray.map pp_value values)))
  | Set values ->
      Printf.sprintf "Set [%s]"
        (String.concat "; " (Iarray.to_list (Iarray.map pp_value values)))
  | Map entries ->
      let pp_entry (key, value) =
        Printf.sprintf "%s => %s" (pp_value key) (pp_value value)
      in
      Printf.sprintf "Map [%s]"
        (String.concat "; " (Iarray.to_list (Iarray.map pp_entry entries)))
  | Tagged (tag, value) -> Printf.sprintf "Tagged (%S, %s)" tag (pp_value value)

let check_value name expected actual =
  if expected <> actual then
    failf "%s\nexpected: %s\nactual:   %s" name (pp_value expected)
      (pp_value actual)

let check_string name expected actual =
  if expected <> actual then
    failf "%s\nexpected: %S\nactual:   %S" name expected actual

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
  | exception exn ->
      failf "%s\nunexpected exception: %s" name (Printexc.to_string exn)
  | _ -> failf "%s\nexpected an exception" name

let check_parse_error name expected f =
  match f () with
  | exception Parse_error message -> check_string name expected message
  | exception exn ->
      failf "%s\nunexpected exception: %s" name (Printexc.to_string exn)
  | _ -> failf "%s\nexpected a Parse_error" name

let test_gadt_constructors () =
  let typed_bool : bool Edn_ocaml.t = Bool true in
  let typed_int : number Edn_ocaml.t = Int 42L in
  let typed_bigint : number Edn_ocaml.t = Bigint "42" in
  let typed_float : number Edn_ocaml.t = Float 42.5 in
  let typed_decimal : number Edn_ocaml.t = Decimal "42.5" in
  let typed_string : string Edn_ocaml.t = String "typed" in
  let typed_symbol : symbol Edn_ocaml.t = Symbol "typed/symbol" in
  let typed_keyword : keyword Edn_ocaml.t = Keyword "typed/keyword" in
  let typed_list : list_ Edn_ocaml.t =
    List (ia [ v typed_symbol; v typed_keyword ])
  in
  let typed_set : set Edn_ocaml.t =
    Set (ia [ v typed_int; v typed_bigint; v typed_float; v typed_decimal ])
  in
  let typed_map : map Edn_ocaml.t =
    Map (ia [ (v typed_keyword, v typed_list) ])
  in
  let typed_vector : vector Edn_ocaml.t =
    Vector (ia [ v typed_bool; v typed_int; v typed_string ])
  in
  check_string "typed constructors write through existential value"
    {|[true 42 "typed"]|}
    (to_edn_string (v typed_vector));
  check_string "typed collection constructors are distinct"
    {|{:typed/keyword (typed/symbol :typed/keyword)}|}
    (to_edn_string (v typed_map));
  check_string "typed set constructor is distinct" {|#{42 42N 42.5 42.5M}|}
    (to_edn_string (v typed_set))

let test_parse_atoms () =
  check_value "nil" nil (of_edn_string "nil");
  check_value "true" (bool true) (of_edn_string "true");
  check_value "false" (bool false) (of_edn_string "false");
  check_value "string escapes" (string "a\tb\n\"c\"\\")
    (of_edn_string {| "a\tb\n\"c\"\\" |});
  check_value "unicode string escape"
    (string "snowman: \226\152\131")
    (of_edn_string {| "snowman: \u2603" |});
  check_value "character" (char (Uchar.of_char 'x')) (of_edn_string {|\x|});
  check_value "named character"
    (char (Uchar.of_char ' '))
    (of_edn_string {|\space|});
  check_value "unicode character"
    (char (Uchar.of_int 0x2603))
    (of_edn_string {|\u2603|});
  check_value "symbol" (symbol "my.ns/name") (of_edn_string "my.ns/name");
  check_value "keyword" (keyword "my.ns/name") (of_edn_string ":my.ns/name");
  check_value "int" (int 42L) (of_edn_string "+42");
  check_value "negative zero" (int 0L) (of_edn_string "-0");
  check_value "big int"
    (bigint "123456789012345678901234567890")
    (of_edn_string "123456789012345678901234567890N");
  check_value "float" (float 6.02e23) (of_edn_string "6.02e23");
  check_value "decimal" (decimal "1.20") (of_edn_string "1.20M")

let test_parse_collections_comments_and_discard () =
  let source =
    {|
      [a b #_foo 42 ; comments run to the end of the line
       {:a 1, "b" [true nil] :c #{foo \space}}]
    |}
  in
  check_value "collections, comments, commas, discard"
    (vector
       [
         symbol "a";
         symbol "b";
         int 42L;
         map
           [
             (keyword "a", int 1L);
             (string "b", vector [ bool true; nil ]);
             (keyword "c", set [ symbol "foo"; char (Uchar.of_char ' ') ]);
           ];
       ])
    (of_edn_string source)

let test_tags_and_streaming () =
  check_value "tagged value"
    (tagged "inst" (string "1985-04-12T23:20:50.52Z"))
    (of_edn_string {|#inst "1985-04-12T23:20:50.52Z"|});
  check_value "read all values"
    (list [ int 1L; int 2L; keyword "done" ])
    (list (of_edn_string_all "1 #_ignored 2 :done"))

let test_writer () =
  check_string "write atoms"
    {|[nil true false "a\nb" \space :k sym 42 1.5 123N 1.20M]|}
    (to_edn_string
       (vector
          [
            nil;
            bool true;
            bool false;
            string "a\nb";
            char (Uchar.of_char ' ');
            keyword "k";
            symbol "sym";
            int 42L;
            float 1.5;
            bigint "123";
            decimal "1.20";
          ]));
  check_string "write collections"
    {|{:a 1 "b" [true nil] :s #{x y} :tag #my/app {:ok true}}|}
    (to_edn_string
       (map
          [
            (keyword "a", int 1L);
            (string "b", vector [ bool true; nil ]);
            (keyword "s", set [ symbol "x"; symbol "y" ]);
            (keyword "tag", tagged "my/app" (map [ (keyword "ok", bool true) ]));
          ]))

let test_json_conversion () =
  let edn =
    map
      [
        (string "name", string "Ada");
        (string "age", int 37L);
        (string "admin", bool false);
        (string "tags", vector [ string "ocaml"; nil ]);
      ]
  in
  check_value "read json string" edn
    (of_json_string
       {|{"name":"Ada","age":37,"admin":false,"tags":["ocaml",null]}|});
  check_json "write json string"
    {|{"name":"Ada","age":37,"admin":false,"tags":["ocaml",null]}|}
    (to_json_string edn);
  check_json "keyword map keys become json object names" {|{"ok":true}|}
    (to_json_string (map [ (keyword "ok", bool true) ]))

let test_errors () =
  check_raises "reject trailing forms" (fun () -> ignore (of_edn_string "1 2"));
  check_raises "reject odd map entry count" (fun () ->
      ignore (of_edn_string "{:a 1 :b}"));
  check_raises "reject mismatched closing delimiter" (fun () ->
      ignore (of_edn_string "{:a/b [1 2 3])"));
  check_parse_error "parse error includes position"
    "at position 13: unexpected closing delimiter: )" (fun () ->
      ignore (of_edn_string "{:a/b [1 2 3])"));
  check_raises "reject invalid keyword" (fun () ->
      ignore (of_edn_string "::bad"));
  check_raises "reject discard without value" (fun () ->
      ignore (of_edn_string "[1 #_]"));
  check_raises "reject non-string json object keys" (fun () ->
      ignore
        (to_json_string
           (map [ (vector [ string "not"; string "a"; string "key" ], int 1L) ])))

let tests =
  [
    ("gadt constructors", test_gadt_constructors);
    ("parse atoms", test_parse_atoms);
    ( "parse collections, comments, and discard",
      test_parse_collections_comments_and_discard );
    ("tags and streaming", test_tags_and_streaming);
    ("writer", test_writer);
    ("json conversion", test_json_conversion);
    ("errors", test_errors);
  ]

let () =
  List.iter
    (fun (name, test) ->
      try test ()
      with exn ->
        Printf.eprintf "FAILED: %s\n%s\n" name (Printexc.to_string exn);
        exit 1)
    tests;
  Printf.printf "ok - %d tests\n" (List.length tests)
