open Core

type t =
  { name : string
  ; args : Dest.t list
  ; ret_type : Bril_type.t option
  ; instrs : Instr.t list
  }
[@@deriving compare, equal, sexp_of]

let of_json json =
  let open Yojson.Basic.Util in
  let arg_of_json json =
    json |> member "name" |> to_string, json |> member "type" |> Bril_type.of_json
  in
  let name = json |> member "name" |> to_string in
  let args = json |> member "args" |> Common.to_list_nonnull |> List.map ~f:arg_of_json in
  let ret_type = json |> member "type" |> Bril_type.of_json_opt in
  let instrs =
    json |> member "instrs" |> Common.to_list_nonnull |> List.map ~f:Instr.of_json
  in
  { name; args; ret_type; instrs }
;;
