type typ =
  | TProp
  | TVar of string
  | TProd of string * typ * typ
  [@@deriving show]

type expr =
  | EVar of string
  | EAbs of string * typ * expr
  | EApp of expr * expr
  [@@deriving show]

let rec free bound v = function
  | TProp -> false
  | TVar n -> n = v && not (List.mem n bound)
  | TProd (n, t, t') -> free bound v t || free (n :: bound) v t'

(*
(* FIXME ça devrait aussi marcher *)
let rec free v = function
  | TProp -> false
  | TVar n -> n = v
  | TProd (n, t, t') -> free v t || n <> v && free v t'
*)

(*
 (fun x -> fun y -> xy) y --> fun z -> yz
 donc en substituant faut pas le paramètre (à côté de fun) apparaisse libre
 dans la valeur fournie, sinon on casse les flèches
 *)

let fresh =
  let ct = ref 0 in
  fun () ->
    incr ct;
    Printf.sprintf "v%d" !ct

(* arf quand on a de vrais types dépendants, l'env est partagé *)

let rec subst env = function
  | TProp -> TProp
  | TVar n' -> if List.mem_assoc n' env then List.assoc n' env else TVar n'
  | TProd (n, t, t') ->
    let n' = fresh () in
    TProd (n', subst env t, subst ((n, TVar n') :: env) t')

(* FIXME bool pour emballer avec des parenthèses si nécessaire *)
let rec pp_coq f = function
  | TProp -> Format.fprintf f "Prop"
  | TVar n -> Format.fprintf f "%s" n
  | TProd (n, t, t') ->
    if free [] n t' then
      Format.fprintf f "forall %s : %a, %a" n pp_coq t pp_coq t'
    else
      Format.fprintf f "%a -> %a" pp_coq t pp_coq t'

let pp_env fmt =
  let open Format in
  let f fmt (n, t) = fprintf fmt "%s : %a" n pp_coq t in
  fprintf fmt "@[<h>%a@]"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") f)

let rec infer env = function
  | EVar n -> List.assoc n env
  | EAbs (n, t, b) -> TProd (n, t, infer ((n, t) :: env) b)
  | EApp (f, p) ->
    let pt' = infer env p in
    match infer env f with
    | TProd (n, (* FIXME *) _pt, bt) -> subst ((n, pt') :: env) bt
    | _ -> failwith "bad"

let rec pp_fr env f e =
  (*
  Format.printf "env=%a@;" pp_env env;
  *)
  match e with
  | EVar n -> Format.fprintf f "Par %s, on a %a.@;" n pp_coq (List.assoc n env)
  | EAbs (n, TProp, b) ->
    Format.fprintf f "Soit %s.@;@[<v 2>%a@]@;Nous avons bien %a.@;"
      n
      (pp_fr ((n, TProp) :: env)) b
      pp_coq (infer env e)
  | EAbs (n, t, b) ->
    Format.fprintf f "Supposons %a (%s).@;@[<v 2>%a@]@;Nous avons bien %a.@;"
      pp_coq t n
      (pp_fr ((n, t) :: env)) b
      pp_coq (infer env e)
  | EApp (e, e') ->
    let ft = infer env e in
    match ft with
    | TProd (n, (* FIXME *) _t, t') ->
      if free [] n t' then
        Format.fprintf f "%aEn particulier, on a %a.@;"
          (pp_fr env) e
          pp_coq t'
      else
        Format.fprintf f "%a%aOn en déduit %a.@;"
          (pp_fr env) e'
          (pp_fr env) e
          pp_coq t'
    | _ -> failwith "meh"
