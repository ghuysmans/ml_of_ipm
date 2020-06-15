type t =
  | Var of string
  | Cons of t * t
  | Let of string * string * t * t
  | Abs of string * t
  | App of t * t
  | Left of t
  | Right of t
  | Match of t * string * t * string * t
  | ExFalso of t

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
  | ExFalso t -> free v t

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
      | ImpI, Hyp ->
        failwith "unbound parameter"
      | ImpI, Out ->
        let a = fresh () in
        let t, k', _env = aux (((node, Hyp), a) :: env) (i In) (fun x -> x) in
        Abs (a, k' t), k, env
      | ImpI, _ -> failwith "invalid output for ImpI"
      | ImpE, Out ->
        (* FIXME mooooonad! *)
        let f, k, env = aux env (i In1) k in
        let x, k, env = aux env (i In2) k in
        App (f, x), k, env
      | ImpE, _ -> failwith "invalid output for ImpE"
      | DisjI1, Out ->
        let t, k, env = aux env (i In) k in
        Left t, k, env
      | DisjI1, _ -> failwith "invalid output for DisjI1"
      | DisjI2, Out ->
        let t, k, env = aux env (i In) k in
        Left t, k, env
      | DisjI2, _ -> failwith "invalid output for DisjI2"
      | DisjE, Hyp1
      | DisjE, Hyp2 ->
        failwith "unbound parameter"
      | DisjE, Out ->
        let t, k, env = aux env (i In) k in
        let l = fresh () in
        let lt, lk, _env = aux (((node, Hyp1), l) :: env) (i In1) (fun x -> x) in
        let r = fresh () in
        let rt, rk, _env = aux (((node, Hyp2), r) :: env) (i In2) (fun x -> x) in
        Match (t, l, lk lt, r, rk rt), k, env
      | FalseE, Out ->
        let t, k', _env = aux env (i In) (fun x -> ExFalso (k x)) in
        t, k', env
      | FalseE, _ -> failwith "invalid output for FalseE"
      | DisjE, _ -> failwith "invalid output for DisjE"
      | Conclusion _, _ -> failwith "invalid output for Conclusion"
      | Assumption p, Out -> Var p (* FIXME *), k, env
      | Assumption _, _ -> failwith "invalid output for Assumption"
      | ConjI, Out ->
        (* FIXME mooooonad! *)
        let l, k, env = aux env (i In1) k in
        let r, k, env = aux env (i In2) k in
        Cons (l, r), k, env
      | ConjI, _ -> failwith "invalid output for ConjI"
      | ConjE, _ ->
        let s, k, env = aux env (i In) k in
        let left = fresh () in
        let right = fresh () in
        let k x = k (Let (left, right, s, x)) in
        let env = ((node, Out1), left) :: ((node, Out2), right) :: env in
        match port with
        | Out1 -> Var left, k, env
        | Out2 -> Var right, k, env
        | _ -> failwith "invalid output for ConjE"
  in
  let t, k, _env = aux [] (pred edges {node; port = In}) (fun x -> x) in
  k t
