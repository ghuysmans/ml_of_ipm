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
  let fresh =
    let ct = ref 0 in
    fun () ->
      incr ct;
      Printf.sprintf "v%d" !ct
  in
  let rec aux env {node; (* output *) port} k =
    let i port = pred edges {node; port} in
    match List.assoc_opt (node, port) env with
    | Some v -> Var v, k, env
    | None ->
      match List.assoc node nodes, port with
      | Conclusion _, _ -> failwith "invalid output for Conclusion"
      | Assumption i, 1 -> Var (Printf.sprintf "a%d" i), k, env
      | Assumption _, _ -> failwith "invalid output for Assumption"
      | ConjI, 1 ->
        (* FIXME mooooonad! *)
        let l, k, env = aux env (i 1) k in
        let r, k, env = aux env (i 2) k in
        Cons (l, r), k, env
      | ConjI, _ -> failwith "invalid output for ConjI"
      | ConjE, _ ->
        let s, k, env = aux env (i 1) k in
        let left = fresh () in
        let right = fresh () in
        let k x = k (Let (left, right, s, x)) in
        let env = ((node, 1), left) :: ((node, 2), right) :: env in
        match port with
        | 1 -> Var left, k, env
        | 2 -> Var right, k, env
        | _ -> failwith "invalid output for ConjE"
  in
  let t, k, _env = aux [] (pred edges {node; port = 1}) (fun x -> x) in
  k t
