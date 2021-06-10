open Core

type t = {
  name : string;
  args : Dest.t list;
  ret_type : Bril_type.t option;
  instrs : Instr.t list;
}
[@@deriving compare, equal, sexp_of]

let of_json json =
  let open Yojson.Basic.Util in
  let arg_of_json json =
    ( json |> member "name" |> to_string,
      json |> member "type" |> Bril_type.of_json )
  in
  let name = json |> member "name" |> to_string in
  let args =
    json |> member "args" |> Common.to_list_nonnull |> List.map ~f:arg_of_json
  in
  let ret_type = json |> member "type" |> Bril_type.of_json_opt in
  let instrs =
    json |> member "instrs" |> Common.to_list_nonnull
    |> List.map ~f:Instr.of_json
  in
  { name; args; ret_type; instrs }

let to_json f =
  `Assoc
    [
      ("name", `String f.name);
      ( "args",
        `List
          (List.map f.args ~f:(fun (name, bril_type) ->
               `Assoc
                 [
                   ("name", `String name); ("type", Bril_type.to_json bril_type);
                 ])) );
      ("instrs", `List (f.instrs |> List.map ~f:Instr.to_json));
    ]

let to_string { name; args; ret_type; instrs } =
  let header =
    sprintf "@%s%s%s {" name
      (match args with
      | [] -> ""
      | args ->
          sprintf "(%s)"
            (List.map args ~f:(fun (name, typ) ->
                 sprintf "%s: %s" name (Bril_type.to_string typ))
            |> String.concat ~sep:", "))
      (Option.value_map ret_type ~default:"" ~f:Bril_type.to_string)
  in
  let body =
    List.map instrs ~f:(fun instr ->
        sprintf
          (match instr with Instr.Label _ -> "%s:" | _ -> "  %s;")
          (Instr.to_string instr))
    |> String.concat ~sep:"\n"
  in
  sprintf "%s\n%s\n}" header body
