type prop = {
  assumptions: Prop.t list;
  conclusions: Prop.t list;
}

let pp ppf {assumptions; conclusions} =
  let pp_sep ppf () = Format.fprintf ppf ",@;" in
  let f = Format.(pp_print_list ~pp_sep Prop.pp) in
  Format.fprintf ppf "@[<hov>%a |- %a@]" f assumptions f conclusions

type save = (prop * Yojson.Safe.t) list

module U = Yojson.Safe.Util

let of_yojson t : save =
  U.member "saved" t |> U.to_assoc |> List.map (fun (k, p) ->
    match Yojson.Safe.from_string k |> U.to_list with
    | [typ; assumptions; conclusions] when U.to_string typ = "predicate" ->
      let parse s = Prop_parser.parse Prop_lexer.top (Lexing.from_string s) in
      let conv = U.(convert_each (fun x -> to_string x |> parse)) in
      let assumptions = conv assumptions in
      let conclusions = conv conclusions in
      [{assumptions; conclusions}, p]
    | _ -> []
  ) |>
  List.flatten
