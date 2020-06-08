type prop = {
  assumptions: string list;
  conclusions: string list;
}

let pp ppf _ (* {assumptions; conclusions} *) =
  Format.fprintf ppf "(todo)"

type save = (prop * Graph.t) list

module U = Yojson.Safe.Util

let of_yojson t : save =
  U.member "saved" t |> U.to_assoc |> List.map (fun (k, p) ->
    match Yojson.Safe.from_string k |> U.to_list with
    | [typ; assumptions; conclusions] when U.to_string typ = "predicate" ->
      let conv = U.(convert_each to_string) in
      let assumptions = conv assumptions in
      let conclusions = conv conclusions in
      (match Graph.of_yojson p with
      | Some p -> [{assumptions; conclusions}, p]
      | None -> [])
    | _ -> []
  ) |>
  List.flatten
