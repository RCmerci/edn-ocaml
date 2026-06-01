type keyword
type symbol
type map
type set
type vector
type list_
type number

type _ t =
  | Nil : unit t
  | Bool : bool -> bool t
  | String : string -> string t
  | Char : Uchar.t -> Uchar.t t
  | Symbol : string -> symbol t
  | Keyword : string -> keyword t
  | Int : int64 -> number t
  | Bigint : string -> number t
  | Float : float -> number t
  | Decimal : string -> number t
  | List : any iarray -> list_ t
  | Vector : any iarray -> vector t
  | Map : (any * any) iarray -> map t
  | Set : any iarray -> set t
  | Tagged : string * any -> (string * any) t

and any = Any : _ t -> any

let any value = Any value

exception Parse_error of string

module Parser = struct
  type parser = { source : string; mutable pos : int; len : int }

  let create source = { source; pos = 0; len = String.length source }
  let is_eof parser = parser.pos >= parser.len

  let peek parser =
    if is_eof parser then None else Some parser.source.[parser.pos]

  let parse_error parser message =
    raise (Parse_error (Printf.sprintf "at position %d: %s" parser.pos message))

  let next parser =
    match peek parser with
    | None -> parse_error parser "unexpected end of input"
    | Some ch ->
        parser.pos <- parser.pos + 1;
        ch

  let is_whitespace = function
    | ' ' | '\n' | '\r' | '\t' | ',' -> true
    | _ -> false

  let rec skip_ws parser =
    match peek parser with
    | Some ch when is_whitespace ch ->
        parser.pos <- parser.pos + 1;
        skip_ws parser
    | Some ';' ->
        skip_comment parser;
        skip_ws parser
    | _ -> ()

  and skip_comment parser =
    match peek parser with
    | None -> ()
    | Some '\n' -> ()
    | Some _ ->
        parser.pos <- parser.pos + 1;
        skip_comment parser

  let is_delimiter = function
    | '"' | '(' | ')' | '[' | ']' | '{' | '}' | ';' -> true
    | ch when is_whitespace ch -> true
    | _ -> false

  let read_hex4 parser =
    if parser.pos + 4 > parser.len then
      parse_error parser "incomplete unicode escape";
    let code = ref 0 in
    for _ = 1 to 4 do
      let ch = next parser in
      let digit =
        match ch with
        | '0' .. '9' -> Char.code ch - Char.code '0'
        | 'a' .. 'f' -> 10 + Char.code ch - Char.code 'a'
        | 'A' .. 'F' -> 10 + Char.code ch - Char.code 'A'
        | _ -> parse_error parser "invalid unicode escape"
      in
      code := (!code * 16) + digit
    done;
    match Uchar.of_int !code with
    | uchar -> uchar
    | exception Invalid_argument _ ->
        parse_error parser "invalid unicode scalar"

  let read_string parser =
    let buffer = Buffer.create 32 in
    let rec loop () =
      match next parser with
      | '"' -> any (String (Buffer.contents buffer))
      | '\\' -> (
          match next parser with
          | 't' ->
              Buffer.add_char buffer '\t';
              loop ()
          | 'r' ->
              Buffer.add_char buffer '\r';
              loop ()
          | 'n' ->
              Buffer.add_char buffer '\n';
              loop ()
          | '\\' ->
              Buffer.add_char buffer '\\';
              loop ()
          | '"' ->
              Buffer.add_char buffer '"';
              loop ()
          | 'u' ->
              Buffer.add_utf_8_uchar buffer (read_hex4 parser);
              loop ()
          | ch ->
              parse_error parser
                (Printf.sprintf "unsupported string escape: \\%c" ch))
      | ch ->
          Buffer.add_char buffer ch;
          loop ()
    in
    loop ()

  let read_token parser =
    let start = parser.pos in
    let rec loop () =
      match peek parser with
      | Some ch when not (is_delimiter ch) ->
          parser.pos <- parser.pos + 1;
          loop ()
      | _ -> String.sub parser.source start (parser.pos - start)
    in
    loop ()

  let full_match pattern token =
    Str.string_match (Str.regexp pattern) token 0
    && Str.match_end () = String.length token

  let integer_re = {|[+-]?\(0\|[1-9][0-9]*\)|}
  let float_re = {|[+-]?\(0\|[1-9][0-9]*\)\(\.[0-9]+\)?\([eE][+-]?[0-9]+\)?|}

  let has_float_marker token =
    String.exists (function '.' | 'e' | 'E' -> true | _ -> false) token

  let strip_leading_plus value =
    if String.length value > 0 && value.[0] = '+' then
      String.sub value 1 (String.length value - 1)
    else value

  let normalize_numeric_literal value =
    match strip_leading_plus value with "-0" -> "0" | value -> value

  let parse_number token =
    let len = String.length token in
    if len > 1 && token.[len - 1] = 'N' then
      let body = String.sub token 0 (len - 1) in
      if full_match integer_re body then
        Some (any (Bigint (normalize_numeric_literal body)))
      else None
    else if len > 1 && token.[len - 1] = 'M' then
      let body = String.sub token 0 (len - 1) in
      if full_match float_re body then
        Some (any (Decimal (normalize_numeric_literal body)))
      else None
    else if has_float_marker token && full_match float_re token then
      Some (any (Float (float_of_string token)))
    else if full_match integer_re token then
      Some (any (Int (Int64.of_string token)))
    else None

  let parse_token parser token =
    match token with
    | "" -> parse_error parser "expected token"
    | "nil" -> any Nil
    | "true" -> any (Bool true)
    | "false" -> any (Bool false)
    | _ -> (
        match parse_number token with
        | Some value -> value
        | None when String.length token > 0 && token.[0] = ':' ->
            if
              String.length token = 1
              || (String.length token > 1 && token.[1] = ':')
            then parse_error parser ("invalid keyword: " ^ token)
            else any (Keyword (String.sub token 1 (String.length token - 1)))
        | None -> any (Symbol token))

  let read_char parser =
    let token = read_token parser in
    match token with
    | "" -> parse_error parser "missing character literal"
    | "newline" -> any (Char (Uchar.of_char '\n'))
    | "return" -> any (Char (Uchar.of_char '\r'))
    | "space" -> any (Char (Uchar.of_char ' '))
    | "tab" -> any (Char (Uchar.of_char '\t'))
    | _ when String.length token = 5 && token.[0] = 'u' ->
        let char_parser = create (String.sub token 1 4) in
        any (Char (read_hex4 char_parser))
    | _ when String.length token = 1 -> any (Char (Uchar.of_char token.[0]))
    | _ -> parse_error parser ("invalid character literal: \\" ^ token)

  let rec read_required parser =
    match read_value parser with
    | Some value -> value
    | None -> parse_error parser "expected EDN value"

  and read_value parser =
    skip_ws parser;
    match peek parser with
    | None -> None
    | Some (')' | ']' | '}') -> None
    | Some '"' ->
        ignore (next parser);
        Some (read_string parser)
    | Some '\\' ->
        ignore (next parser);
        Some (read_char parser)
    | Some '(' ->
        ignore (next parser);
        Some (any (List (Iarray.of_list (read_sequence parser ')'))))
    | Some '[' ->
        ignore (next parser);
        Some (any (Vector (Iarray.of_list (read_sequence parser ']'))))
    | Some '{' ->
        ignore (next parser);
        Some (read_map parser)
    | Some '#' ->
        ignore (next parser);
        read_dispatch parser
    | Some _ -> Some (parse_token parser (read_token parser))

  and read_sequence parser closing =
    let rec loop acc =
      skip_ws parser;
      match peek parser with
      | Some ch when ch = closing ->
          ignore (next parser);
          List.rev acc
      | Some ((')' | ']' | '}') as ch) ->
          parse_error parser
            (Printf.sprintf "unexpected closing delimiter: %c" ch)
      | None -> parse_error parser (Printf.sprintf "missing closing %c" closing)
      | _ -> (
          match read_value parser with
          | Some value -> loop (value :: acc)
          | None -> loop acc)
    in
    loop []

  and read_map parser =
    let values = read_sequence parser '}' in
    let rec pairs acc = function
      | [] -> any (Map (Iarray.of_list (List.rev acc)))
      | [ _ ] -> parse_error parser "map requires an even number of forms"
      | key :: value :: rest -> pairs ((key, value) :: acc) rest
    in
    pairs [] values

  and read_dispatch parser =
    match peek parser with
    | Some '{' ->
        ignore (next parser);
        Some (any (Set (Iarray.of_list (read_sequence parser '}'))))
    | Some '_' ->
        ignore (next parser);
        ignore (read_required parser);
        read_value parser
    | Some ch when ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z') ->
        let tag = read_token parser in
        let value = read_required parser in
        Some (any (Tagged (tag, value)))
    | Some ch ->
        parse_error parser (Printf.sprintf "unsupported dispatch: #%c" ch)
    | None -> parse_error parser "missing dispatch character"
end

let of_edn_string_all source =
  let parser = Parser.create source in
  let rec loop acc =
    match Parser.read_value parser with
    | Some value -> loop (value :: acc)
    | None ->
        Parser.skip_ws parser;
        if Parser.is_eof parser then List.rev acc
        else Parser.parse_error parser "unexpected closing delimiter"
  in
  loop []

let of_edn_string source =
  let parser = Parser.create source in
  match Parser.read_value parser with
  | None -> Parser.parse_error parser "expected EDN value"
  | Some value ->
      Parser.skip_ws parser;
      if Parser.is_eof parser then value
      else Parser.parse_error parser "expected a single EDN value"

let escape_string value =
  let buffer = Buffer.create (String.length value + 8) in
  let add_escape = function
    | '\t' -> Buffer.add_string buffer "\\t"
    | '\r' -> Buffer.add_string buffer "\\r"
    | '\n' -> Buffer.add_string buffer "\\n"
    | '\\' -> Buffer.add_string buffer "\\\\"
    | '"' -> Buffer.add_string buffer "\\\""
    | ch -> Buffer.add_char buffer ch
  in
  String.iter add_escape value;
  Buffer.contents buffer

let string_of_char_literal uchar =
  if Uchar.equal uchar (Uchar.of_char '\n') then "\\newline"
  else if Uchar.equal uchar (Uchar.of_char '\r') then "\\return"
  else if Uchar.equal uchar (Uchar.of_char ' ') then "\\space"
  else if Uchar.equal uchar (Uchar.of_char '\t') then "\\tab"
  else
    match Uchar.to_char uchar with
    | ch -> Printf.sprintf "\\%c" ch
    | exception Invalid_argument _ ->
        Printf.sprintf "\\u%04X" (Uchar.to_int uchar)

let join_iarray separator render values =
  let buffer = Buffer.create 32 in
  Iarray.iteri
    (fun index value ->
      if index > 0 then Buffer.add_string buffer separator;
      Buffer.add_string buffer (render value))
    values;
  Buffer.contents buffer

let rec to_edn_string (Any value) =
  match value with
  | Nil -> "nil"
  | Bool true -> "true"
  | Bool false -> "false"
  | String value -> "\"" ^ escape_string value ^ "\""
  | Char value -> string_of_char_literal value
  | Symbol value -> value
  | Keyword value -> ":" ^ value
  | Int value -> Int64.to_string value
  | Bigint value -> normalize_number value ^ "N"
  | Float value -> string_of_float value
  | Decimal value -> normalize_number value ^ "M"
  | List values -> "(" ^ join_iarray " " to_edn_string values ^ ")"
  | Vector values -> "[" ^ join_iarray " " to_edn_string values ^ "]"
  | Set values -> "#{" ^ join_iarray " " to_edn_string values ^ "}"
  | Map entries ->
      let render_entry (key, value) =
        to_edn_string key ^ " " ^ to_edn_string value
      in
      "{" ^ join_iarray " " render_entry entries ^ "}"
  | Tagged (tag, value) -> "#" ^ tag ^ " " ^ to_edn_string value

and normalize_number value =
  if value = "-0" || value = "+0" then "0" else Parser.strip_leading_plus value

let rec of_json = function
  | `Null -> any Nil
  | `Bool value -> any (Bool value)
  | `String value -> any (String value)
  | `Int value -> any (Int (Int64.of_int value))
  | `Intlit value -> any (Int (Int64.of_string value))
  | `Float value -> any (Float value)
  | `List values -> any (Vector (Iarray.of_list (List.map of_json values)))
  | `Assoc entries ->
      any
        (Map
           (Iarray.of_list
              (List.map
                 (fun (key, value) -> (any (String key), of_json value))
                 entries)))
  | `Tuple values -> any (Vector (Iarray.of_list (List.map of_json values)))
  | `Variant (name, payload) -> (
      match payload with
      | None -> any (String name)
      | Some value ->
          any (Vector (Iarray.of_list [ any (String name); of_json value ])))

let of_json_string source = Yojson.Safe.from_string source |> of_json

let json_key_of_value (Any value) =
  match value with
  | String value -> value
  | Keyword value -> value
  | Symbol value -> value
  | _ ->
      invalid_arg
        "EDN map contains a key that cannot be encoded as a JSON object name"

let iarray_to_list render values =
  Iarray.fold_right (fun value acc -> render value :: acc) values []

let rec to_json (Any value) =
  match value with
  | Nil -> `Null
  | Bool value -> `Bool value
  | String value -> `String value
  | Char value -> `String (string_of_char_literal value)
  | Symbol value -> `String value
  | Keyword value -> `String (":" ^ value)
  | Int value ->
      if value <= Int64.of_int max_int && value >= Int64.of_int min_int then
        `Int (Int64.to_int value)
      else `Intlit (Int64.to_string value)
  | Bigint value -> `Intlit value
  | Float value -> `Float value
  | Decimal value -> `String value
  | List values | Vector values -> `List (iarray_to_list to_json values)
  | Set values -> `List (iarray_to_list to_json values)
  | Map entries ->
      `Assoc
        (iarray_to_list
           (fun (key, value) -> (json_key_of_value key, to_json value))
           entries)
  | Tagged (tag, value) ->
      `Assoc [ ("tag", `String tag); ("value", to_json value) ]

let to_json_string value = Yojson.Safe.to_string (to_json value)
