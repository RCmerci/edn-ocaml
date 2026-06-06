open Edn_ocaml

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

let () =
  Jest.describe "EDN reader/writer" (fun () ->
      Jest.describe "GADT constructors" (fun () ->
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
            Set
              (ia
                 [ v typed_int; v typed_bigint; v typed_float; v typed_decimal ])
          in
          let typed_map : map Edn_ocaml.t =
            Map (ia [ (v typed_keyword, v typed_list) ])
          in
          let typed_vector : vector Edn_ocaml.t =
            Vector (ia [ v typed_bool; v typed_int; v typed_string ])
          in
          Jest.test "writes existential typed vectors" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (v typed_vector))
                |> toEqual {|[true 42 "typed"]|}));
          Jest.test "keeps typed collection constructors distinct" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (v typed_map))
                |> toEqual {|{:typed/keyword (typed/symbol :typed/keyword)}|}));
          Jest.test "keeps typed set constructor distinct" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (v typed_set))
                |> toEqual {|#{42 42N 42.5 42.5M}|})));
      Jest.describe "EDN parsing" (fun () ->
          Jest.test "parses nil" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (of_edn_string "nil")) |> toEqual "nil"));
          Jest.test "parses booleans" (fun () ->
              Jest.Expect.(
                expect
                  [|
                    to_edn_string (of_edn_string "true");
                    to_edn_string (of_edn_string "false");
                  |]
                |> toEqual [| "true"; "false" |]));
          Jest.test "parses string escapes" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (of_edn_string {| "a\tb\n\"c\"\\" |}))
                |> toEqual {|"a\tb\n\"c\"\\"|}));
          Jest.test "parses unicode string escapes" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (of_edn_string {| "snowman: \u2603" |}))
                |> toEqual {|"snowman: ☃"|}));
          Jest.test "parses character literals" (fun () ->
              Jest.Expect.(
                expect
                  [|
                    to_edn_string (of_edn_string {|\x|});
                    to_edn_string (of_edn_string {|\space|});
                    to_edn_string (of_edn_string {|\u2603|});
                  |]
                |> toEqual [| {|\x|}; {|\space|}; {|\u2603|} |]));
          Jest.test "parses symbols and keywords" (fun () ->
              Jest.Expect.(
                expect
                  [|
                    to_edn_string (of_edn_string "my.ns/name");
                    to_edn_string (of_edn_string ":my.ns/name");
                  |]
                |> toEqual [| "my.ns/name"; ":my.ns/name" |]));
          Jest.test "parses numeric literals" (fun () ->
              Jest.Expect.(
                expect
                  [|
                    to_edn_string (of_edn_string "+42");
                    to_edn_string (of_edn_string "-0");
                    to_edn_string
                      (of_edn_string "123456789012345678901234567890N");
                    to_edn_string (of_edn_string "6.02e23");
                    to_edn_string (of_edn_string "1.20M");
                  |]
                |> toEqual
                     [|
                       "42";
                       "0";
                       "123456789012345678901234567890N";
                       "6.02e+23";
                       "1.20M";
                     |]));
          Jest.test "keeps malformed numeric tokens as symbols" (fun () ->
              Jest.Expect.(
                expect
                  [|
                    to_edn_string (of_edn_string "1.");
                    to_edn_string (of_edn_string "1e");
                  |]
                |> toEqual [| "1."; "1e" |]));
          Jest.test "parses collections, comments, commas, and discard"
            (fun () ->
              let source =
                {|
      [a b #_foo 42 ; comments run to the end of the line
       {:a 1, "b" [true nil] :c #{foo \space}}]
    |}
              in
              Jest.Expect.(
                expect (to_edn_string (of_edn_string source))
                |> toEqual {|[a b 42 {:a 1 "b" [true nil] :c #{foo \space}}]|}));
          Jest.test "parses tagged values" (fun () ->
              Jest.Expect.(
                expect
                  (to_edn_string
                     (of_edn_string {|#inst "1985-04-12T23:20:50.52Z"|}))
                |> toEqual {|#inst "1985-04-12T23:20:50.52Z"|}));
          Jest.test "reads all values" (fun () ->
              Jest.Expect.(
                expect
                  (to_edn_string
                     (list (of_edn_string_all "1 #_ignored 2 :done")))
                |> toEqual "(1 2 :done)"));
          Jest.describe "EDN writing" (fun () ->
              Jest.test "writes atoms" (fun () ->
                  Jest.Expect.(
                    expect
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
                            ]))
                    |> toEqual
                         {|[nil true false "a\nb" \space :k sym 42 1.5 123N 1.20M]|}));
              Jest.test "writes collections" (fun () ->
                  Jest.Expect.(
                    expect
                      (to_edn_string
                         (map
                            [
                              (keyword "a", int 1L);
                              (string "b", vector [ bool true; nil ]);
                              (keyword "s", set [ symbol "x"; symbol "y" ]);
                              ( keyword "tag",
                                tagged "my/app"
                                  (map [ (keyword "ok", bool true) ]) );
                            ]))
                    |> toEqual
                         {|{:a 1 "b" [true nil] :s #{x y} :tag #my/app {:ok true}}|})));
          Jest.describe "JSON conversion" (fun () ->
              let edn =
                map
                  [
                    (string "name", string "Ada");
                    (string "age", int 37L);
                    (string "large", int 3000000000L);
                    (string "admin", bool false);
                    (string "tags", vector [ string "ocaml"; nil ]);
                  ]
              in
              Jest.test "reads JSON strings" (fun () ->
                  Jest.Expect.(
                    expect
                      (to_edn_string
                         (of_json_string
                            {|{"name":"Ada","age":37,"large":3000000000,"admin":false,"tags":["ocaml",null]}|}))
                    |> toEqual (to_edn_string edn)));
              Jest.test "writes JSON strings" (fun () ->
                  Jest.Expect.(
                    expect (to_json_string edn)
                    |> toEqual
                         {|{"name":"Ada","age":37,"large":3000000000,"admin":false,"tags":["ocaml",null]}|}));
              Jest.test "writes keyword map keys as JSON object names"
                (fun () ->
                  Jest.Expect.(
                    expect (to_json_string (map [ (keyword "ok", bool true) ]))
                    |> toEqual {|{"ok":true}|})));
          Jest.describe "errors" (fun () ->
              Jest.test "rejects trailing forms" (fun () ->
                  Jest.Expect.(
                    expectFn (fun () -> ignore (of_edn_string "1 2")) ()
                    |> toThrow));
              Jest.test "rejects odd map entry count" (fun () ->
                  Jest.Expect.(
                    expectFn (fun () -> ignore (of_edn_string "{:a 1 :b}")) ()
                    |> toThrow));
              Jest.test "rejects mismatched closing delimiter" (fun () ->
                  Jest.Expect.(
                    expectFn
                      (fun () -> ignore (of_edn_string "{:a/b [1 2 3])"))
                      ()
                    |> toThrow));
              Jest.test "rejects invalid keyword" (fun () ->
                  Jest.Expect.(
                    expectFn (fun () -> ignore (of_edn_string "::bad")) ()
                    |> toThrow));
              Jest.test "rejects discard without value" (fun () ->
                  Jest.Expect.(
                    expectFn (fun () -> ignore (of_edn_string "[1 #_]")) ()
                    |> toThrow));
              Jest.test "rejects non-string JSON object keys" (fun () ->
                  Jest.Expect.(
                    expectFn
                      (fun () ->
                        ignore
                          (to_json_string
                             (map
                                [
                                  ( vector
                                      [ string "not"; string "a"; string "key" ],
                                    int 1L );
                                ])))
                      ()
                    |> toThrow)))))
