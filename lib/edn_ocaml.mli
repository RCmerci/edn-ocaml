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

exception Parse_error of string

val of_edn_string : string -> any
val of_edn_string_all : string -> any list
val to_edn_string : any -> string
val of_json : Yojson.Safe.t -> any
val of_json_string : string -> any
val to_json : any -> Yojson.Safe.t
val to_json_string : any -> string
