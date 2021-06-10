open Core

type t = IntType | BoolType | PtrType of t
[@@deriving compare, equal, sexp_of]

let rec of_json =
  let open Yojson.Basic.Util in
  function
  | `String "int" -> IntType
  | `String "bool" -> BoolType
  | `Assoc [ ("ptr", t) ] -> PtrType (of_json t)
  | json -> failwithf "invalid type: %s" (to_string json) ()

let of_json_opt = function `Null -> None | json -> Some (of_json json)

let rec to_json = function
  | IntType -> `String "int"
  | BoolType -> `String "bool"
  | PtrType t -> `Assoc [ ("ptr", to_json t) ]

let rec to_string = function
  | IntType -> "int"
  | BoolType -> "bool"
  | PtrType t -> sprintf "ptr<%s>" (to_string t)
