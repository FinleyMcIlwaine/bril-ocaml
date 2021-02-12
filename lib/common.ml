let has_key json k =
  let open Yojson.Basic.Util in
  match member k json with
  | `Null -> false
  | _ -> true
;;

let to_list_nonnull =
  let open Yojson.Basic.Util in
  function
  | `Null -> []
  | json -> to_list json
;;
