type t =
  | Assumption of int
  | Conclusion of int
  | ConjI
  | ConjE
  | ArrowI
  | ArrowE
  | DisjIL
  | DisjIR
  | DisjE
  | FalseE

type dir =
  | In
  | Out

type r = {
  node: string;
  port: int;
}

type edge = {
  source: r;
  dest: r;
  (*
  label: string; (* labels[0].attrs["text"].text *)
  *)
}

let (-->) (a, p) (b, p') = {
  source = {node = a; port = p};
  dest = {node = b; port = p'}
}

let pred g d =
  (g |> List.find (fun {dest; _} -> dest = d)).source

(*
let pp_r f {node; port} =
  Format.fprintf f "(%s, %d)" node port

let pred g d =
  let res = pred g d in
  Format.printf "%a -> %a@;" pp_r res pp_r d;
  res
*)
