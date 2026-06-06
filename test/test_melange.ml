open Edn_ocaml

let () =
  Jest.describe "Melange smoke" (fun () ->
      Jest.test "writes EDN" (fun () ->
          let edn =
            of_edn_string {|{:name "Ada" :tags ["ocaml" nil] :ok true}|}
          in
          Jest.Expect.(
            expect (to_edn_string edn)
            |> toEqual {|{:name "Ada" :tags ["ocaml" nil] :ok true}|}));
      Jest.test "reads JSON strings" (fun () ->
          Jest.Expect.(
            expect
              (to_edn_string
                 (of_json_string
                    {|{"name":"Ada","age":37,"admin":false,"tags":["ocaml",null]}|}))
            |> toEqual
                 {|{"name" "Ada" "age" 37 "admin" false "tags" ["ocaml" nil]}|}));
      Jest.test "writes JSON strings" (fun () ->
          let edn =
            of_edn_string {|{:name "Ada" :tags ["ocaml" nil] :ok true}|}
          in
          Jest.Expect.(
            expect (to_json_string edn)
            |> toEqual {|{"name":"Ada","tags":["ocaml",null],"ok":true}|})))
