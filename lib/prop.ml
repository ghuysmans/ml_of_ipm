type v =
  | Var of string
  | App of string * v list

type t =
  | Prop of char
  | Conj of t * t
  | Disj of t * t
  | Imp of t * t
  | False
  | Forall of string * t
  | Exists of string * t
  | Pred of string * v list

let pp_sep ppf () = Format.fprintf ppf ",@;"

let rec pp_v ppf = function
  | Var v -> Format.fprintf ppf "%s" v
  | App (f, a) ->
    let pp_args = Format.(pp_print_list ~pp_sep pp_v) in
    Format.fprintf ppf "@[<hov>%s(%a)@]" f pp_args a

let rec pp ppf = function
  | Prop p -> Format.fprintf ppf "%c" p
  | Conj (p, q) -> Format.fprintf ppf "@[<hov>(%a /\\@ %a)@]" pp p pp q
  | Disj (p, q) -> Format.fprintf ppf "@[<hov>(%a \\/@ %a)@]" pp p pp q
  | Imp (p, q) -> Format.fprintf ppf "@[<hov>(%a ->@ %a)@]" pp p pp q
  | False -> Format.fprintf ppf "False"
  | Forall (v, p) -> Format.fprintf ppf "@[<hov>(forall %s.@ %a)@]" v pp p
  | Exists (v, p) -> Format.fprintf ppf "@[<hov>(exists %s.@ %a)@]" v pp p
  | Pred (n, a) ->
    let pp_args = Format.(pp_print_list ~pp_sep pp_v) in
    Format.fprintf ppf "@[<hov>%s(%a)@]" n pp_args a
