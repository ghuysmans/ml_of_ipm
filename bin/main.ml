open Compiler
module U = Yojson.Safe.Util

let () =
  let proofs () =
    Yojson.Safe.from_channel stdin |>
    U.member "saved" |> U.to_assoc
  in
  match Sys.argv with
  | [| _; "-l" |] ->
    List.iter (fun (p, _) -> print_endline p) (proofs ())
  | [| _; prop |] ->
    let nodes, edges = List.assoc prop (proofs ()) |> Graph.of_yojson in
    let t = Yojson.Safe.from_string prop in
    (match U.to_list t with
    | [typ; _; conclusions] when U.to_string typ = "predicate" ->
      let conclusions = U.(convert_each to_string conclusions) in
      conclusions |> List.iter (fun c ->
        let c =
          nodes |> List.find (function
            | _, Graph.Conclusion p -> p = c
            | _ -> false
          ) |> fst
        in
        print_endline Expr.(show @@ of_graph (nodes, edges) c)
      )
    | _ ->
      prerr_endline "incompatible proof system";
      exit 1)
  | _ ->
    Printf.eprintf "usage: %s [-l | prop]\n" Sys.argv.(0);
    exit 1
