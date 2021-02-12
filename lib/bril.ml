open Core
module Bril_type = Bril_type
module Const = Const
module Dest = Dest
module Func = Func
module Instr = Instr
module Op = Op

type t = Func.t list [@@deriving compare, equal, sexp_of]

let from_json json =
  let open Yojson.Basic.Util in
  json |> member "functions" |> Common.to_list_nonnull |> List.map ~f:Func.of_json
;;
