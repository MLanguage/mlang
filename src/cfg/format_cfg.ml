open Cfg

let format_typ (t: typ) : string = match t with
  | Integer -> "integer"
  | Real -> "real"
  | Boolean -> "boolean"
