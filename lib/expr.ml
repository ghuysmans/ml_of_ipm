(* TODO hashcons *)
type t =
  | Var of string
  | Fst of t
  | Snd of t
  | Cons of t * t
  [@@deriving show {with_path = false}]
  (*
  | Let of string * t * t

let rec free v = function
  | Var v' -> v = v'
  | Fst t -> free v t
  | Snd t -> free v t
  | Cons (t, t') -> free v t || free v t'
  | Let (v', t, t') -> free v t || v <> v' && free v t'
  *)

open Graph

let of_graph (nodes, edges) node =
  let rec aux env {node; (* output *) port} =
    let i port = pred edges {node; port} in
    match List.assoc node nodes, port with
    | Conclusion _, _ -> failwith "invalid output for Conclusion"
    | Assumption i, 1 -> Var (Printf.sprintf "a%d" i)
    | Assumption _, _ -> failwith "invalid output for Assumption"
    | ConjI, 1 -> Cons (aux env (i 1), aux env (i 2))
    | ConjI, _ -> failwith "invalid output for ConjI"
    | ConjE, 1 -> Fst (aux env (i 1))
    | ConjE, 2 -> Snd (aux env (i 1))
    | ConjE, _ -> failwith "invalid output for ConjE"
  in
  aux [] (pred edges {node; port = 1})
