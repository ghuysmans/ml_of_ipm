type t =
  | Var of string
  | Cons of t * t
  | Let of string * string * t * t
  | Abs of string * t
  | App of t * t
  | Left of t
  | Right of t
  | Match of t * string * t * string * t
  | ExFalso
  [@@deriving show {with_path = false}]

let rec free v = function
  | Var v' -> v = v'
  | Cons (t, t') -> free v t || free v t'
  | Let (l, r, t, b) -> free v t || v <> l && v <> r && free v b
  | Abs (a, b) -> v <> a && free v b
  | App (f, x) -> free v f || free v x
  | Left t -> free v t
  | Right t -> free v t
  | Match (d, l, lt, r, rt) ->
    free v d || v <> l && free v lt || v <> r && free v rt
  | ExFalso -> false

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
      | ArrowI, 1 ->
        failwith "unbound parameter"
      | ArrowI, 2 ->
        let a = fresh () in
        let t, k', _env = aux (((node, 1), a) :: env) (i 1) (fun x -> x) in
        Abs (a, k' t), k, env
      | ArrowI, _ -> failwith "invalid output for ArrowI"
      | ArrowE, 1 ->
        (* FIXME mooooonad! *)
        let f, k, env = aux env (i 1) k in
        let x, k, env = aux env (i 2) k in
        App (f, x), k, env
      | ArrowE, _ -> failwith "invalid output for ArrowE"
      | DisjIL, 1 ->
        let t, k, env = aux env (i 1) k in
        Left t, k, env
      | DisjIL, _ -> failwith "invalid output for DisjIL"
      | DisjIR, 1 ->
        let t, k, env = aux env (i 1) k in
        Left t, k, env
      | DisjIR, _ -> failwith "invalid output for DisjIR"
      | DisjE, 1
      | DisjE, 2 ->
        failwith "unbound parameter"
      | DisjE, 3 ->
        let t, k, env = aux env (i 1) k in
        let l = fresh () in
        let lt, lk, _env = aux (((node, 1), l) :: env) (i 2) (fun x -> x) in
        let r = fresh () in
        let rt, rk, _env = aux (((node, 2), r) :: env) (i 3) (fun x -> x) in
        Match (t, l, lk lt, r, rk rt), k, env
      | FalseE, 1 ->
        ExFalso, k, env
      | FalseE, _ -> failwith "invalid output for FalseE"
      | DisjE, _ -> failwith "invalid output for DisjE"
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
