type t =
  | Assumption of string
  | Conclusion of string
  | ConjI
  | ConjE
  | ImpI
  | ImpE
  | DisjI1
  | DisjI2
  | DisjE
  | FalseE

type port =
  | Out
  | Out1
  | Out2
  | In
  | In1
  | In2
  | Hyp
  | Hyp1
  | Hyp2

let port_of_string = function
  | "out" -> Out
  | "out1" -> Out1
  | "out2" -> Out2
  | "in" -> In
  | "in1" -> In1
  | "in2" -> In2
  | "hyp" -> Hyp
  | "hyp1" -> Hyp1
  | "hyp2" -> Hyp2
  | _ -> failwith "port_of_string"

type r = {
  node: string;
  port: port;
}

type edge = {
  source: r;
  dest: r;
  label: string;
}

let pred g d =
  (g |> List.find (fun {dest; _} -> dest = d)).source

module U = Yojson.Safe.Util

let of_yojson t =
  if U.member "qed" t |> U.to_bool then
    let nodes, edges =
      U.member "cells" t |> U.to_list |> List.map (fun c ->
        let typ = U.member "type" c |> U.to_string in
        if typ = "incredible.Link" then
          (* FIXME ignore disconnected links *)
          let get_end name =
            let e = U.member name c in
            {
              node = U.member "id" e |> U.to_string;
              port = U.member "port" e |> U.to_string |> port_of_string
            }
          in
          let source = get_end "source" in
          let dest = get_end "target" in
          let label =
            U.member "labels" c |> U.index 0 |> U.member "attrs" |>
            U.member "text" |> U.member "text" |> U.to_string
          in
          [], [{source; dest; label}]
        else (* if typ = "incredible.Generic" then *)
          let get_nth typ n =
            U.member "task" c |> U.member typ |>
            U.index (U.to_int n - 1) |>
            U.to_string
          in
          match
            U.to_assoc c |> List.map (function
              | "assumption", n -> [Assumption (get_nth "assumptions" n)]
              | "conclusion", n -> [Conclusion (get_nth "conclusions" n)]
              | "rule", r ->
                (match U.(member "id" r |> to_string) with
                | "conjI" -> [ConjI]
                | "conjE" -> [ConjE]
                | "impI" -> [ImpI]
                | "impE" -> [ImpE]
                | "disjI1" -> [DisjI1]
                | "disjI2" -> [DisjI2]
                | "disjE" -> [DisjE]
                | "falseE" -> [FalseE]
                | name -> failwith @@ "unknown rule " ^ name)
              | _ -> [] (* uninteresting member *)
            ) |>
            List.flatten
          with
          | [x] -> [U.member "id" c |> U.to_string, x], []
          | _ -> failwith "unknown node type"
      ) |> List.split in
    List.flatten nodes, List.flatten edges
  else
    failwith "incomplete or invalid proof"
