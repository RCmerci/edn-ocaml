type t =
  | Nil
  | Bool of bool
  | String of string
  | Char of Uchar.t
  | Symbol of string
  | Keyword of string
  | Int of int64
  | Bigint of string
  | Float of float
  | Decimal of string
  | List of t iarray
  | Vector of t iarray
  | Map of (t * t) iarray
  | Set of t iarray
  | Tagged of string * t

exception Parse_error of string

val of_edn_string : string -> t
val of_edn_string_all : string -> t list
val to_edn_string : t -> string

val of_json : Yojson.Safe.t -> t
val of_json_string : string -> t
val to_json : t -> Yojson.Safe.t
val to_json_string : t -> string
