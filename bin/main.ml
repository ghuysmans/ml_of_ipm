open Compiler

let list fn =
  let f ppf (i, p) = Format.fprintf ppf "%d : %a" (i + 1) Ipm.pp p in
  Ipm.of_yojson @@ Yojson.Safe.from_file fn |> List.mapi (fun i p -> i, p) |>
  Format.(printf "@[<v>%a@]@." (pp_print_list ~pp_sep:pp_print_cut f))

let compile fn i =
  let t = Ipm.of_yojson @@ Yojson.Safe.from_file fn in
  let {Ipm.conclusions; _}, j = List.nth t (i - 1) in
  let nodes, edges = Graph.of_yojson j in
  let f ppf c =
    let c =
      nodes |> List.find (function
        | _, Graph.Conclusion p -> p = c
        | _ -> false
      ) |> fst
    in
    Expr.pp ppf (Expr.of_graph (nodes, edges) c)
  in
  Format.(printf "@[<v>%a@]@." (pp_print_list ~pp_sep:pp_print_cut f) conclusions)


open Cmdliner

let input =
  let doc = "localStorage dump" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"DUMP")

let list_cmd =
  let doc = "list propositions" in
  Term.(const list $ input),
  Term.info "list" ~doc

let nth =
  let doc = "property index" in
  Arg.(required & pos 1 (some int) None & info [] ~doc ~docv:"INDEX")

let compile_cmd =
  let doc = "compile a finished proof" in
  Term.(const compile $ input $ nth),
  Term.info "compile" ~doc

let () =
  let t =
    let doc = "a compiler for the Incredible Proof Machine" in
    Term.(ret @@ const @@ `Help (`Pager, None)),
    Term.info "ml_of_ipm" ~doc
  in
  Term.(exit @@ eval_choice t [
    list_cmd;
    compile_cmd;
  ])
