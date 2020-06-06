type t =
  | Var of string
  | Cons of t * t
  | Let of string * string * t * t
  [@@deriving show {with_path = false}]

let rec free v = function
  | Var v' -> v = v'
  | Cons (t, t') -> free v t || free v t'
  | Let (l, r, t, b) -> free v t || v <> l && v <> r && free v b

open Graph

let of_graph (nodes, edges) node =
  let rec aux env {node; (* output *) port} =
    let i port = pred edges {node; port} in
    match List.assoc_opt (node, port) env with
    | Some v -> Var v
    | None ->
      match List.assoc node nodes, port with
      | Conclusion _, _ -> failwith "invalid output for Conclusion"
      | Assumption i, 1 -> Var (Printf.sprintf "a%d" i)
      | Assumption _, _ -> failwith "invalid output for Assumption"
      | ConjI, 1 -> Cons (aux env (i 1), aux env (i 2))
      | ConjI, _ -> failwith "invalid output for ConjI"
      | ConjE, _ ->
        let env' = ((node, 1), "x") :: ((node, 2), "y") :: env in
        match port with
        | 1 -> Let ("x", "y", aux env' (i 1), Var "x")
        | 2 -> Let ("x", "y", aux env' (i 1), Var "y")
        | _ -> failwith "invalid output for ConjE"
  in
  aux [] (pred edges {node; port = 1})
