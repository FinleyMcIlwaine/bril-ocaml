open Core
open Bril_type

type label = string [@@deriving compare, equal, sexp_of]
type arg = string [@@deriving compare, equal, sexp_of]

type t =
  | Label of label
  | Const of Dest.t * Const.t
  | Binary of Dest.t * Op.Binary.t * arg * arg
  | Unary of Dest.t * Op.Unary.t * arg
  | Jmp of label
  | Br of arg * label * label
  | Call of Dest.t option * string * arg list
  | Ret of arg option
  | Print of arg list
  | Nop
[@@deriving compare, equal, sexp_of]

let to_string =
  let dest_to_string (name, bril_type) =
    sprintf "%s: %s =" name (Bril_type.to_string bril_type)
  in
  function
  | Label label -> sprintf ".%s" label
  | Const (dest, const) ->
    sprintf "%s const %s" (dest_to_string dest) (Const.to_string const)
  | Binary (dest, op, arg1, arg2) ->
    sprintf "%s %s %s %s" (dest_to_string dest) (Op.Binary.to_string op) arg1 arg2
  | Unary (dest, op, arg) ->
    sprintf "%s %s %s" (dest_to_string dest) (Op.Unary.to_string op) arg
  | Jmp label -> sprintf "jmp .%s" label
  | Br (arg, l1, l2) -> sprintf "br %s .%s .%s" arg l1 l2
  | Call (dest, func_name, args) ->
    List.filter
      ([ Option.value_map dest ~default:"" ~f:dest_to_string; func_name ] @ args)
      ~f:(Fn.non String.is_empty)
    |> String.concat ~sep:" "
  | Ret None -> "ret"
  | Ret (Some a) -> sprintf "ret %s" a
  | Print args -> String.concat ~sep:" " ("print" :: args)
  | Nop -> "nop"
;;

let of_json json =
  let open Yojson.Basic.Util in
  match json |> member "label" with
  | `String label -> Label label
  | `Null ->
    let dest () =
      json |> member "dest" |> to_string, json |> member "type" |> Bril_type.of_json
    in
    let args () =
      json |> member "args" |> Common.to_list_nonnull |> List.map ~f:to_string
    in
    let labels () =
      json |> member "labels" |> Common.to_list_nonnull |> List.map ~f:to_string
    in
    let arg = List.nth_exn (args ()) in
    let label = List.nth_exn (labels ()) in
    (match json |> member "op" |> to_string with
    | "const" ->
      let const =
        match json |> member "type" |> Bril_type.of_json with
        | IntType -> Const.Int (json |> member "value" |> to_int)
        | BoolType -> Const.Bool (json |> member "value" |> to_bool)
      in
      Const (dest (), const)
    | op when Op.Binary.is_op op -> Binary (dest (), Op.Binary.of_string op, arg 0, arg 1)
    | op when Op.Unary.is_op op -> Unary (dest (), Op.Unary.of_string op, arg 0)
    | "jmp" -> Jmp (label 0)
    | "br" -> Br (arg 0, label 0, label 1)
    | "call" ->
      Call
        ( (if Common.has_key json "dest" then Some (dest ()) else None)
        , json |> member "funcs" |> Common.to_list_nonnull |> List.hd_exn |> to_string
        , args () )
    | "ret" -> Ret (if List.is_empty (args ()) then None else Some (arg 0))
    | "print" -> Print (args ())
    | "nop" -> Nop
    | op -> failwithf "invalid op: %s" op ())
  | json -> failwithf "invalid label: %s" (to_string json) ()
;;
