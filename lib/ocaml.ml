open Parsetree

let mkcoretype ptyp_desc = {
  ptyp_desc;
  ptyp_loc = Location.none;
  ptyp_attributes = [];
}

let rec of_prop = function
  | Prop.Prop p -> mkcoretype @@
    Ptyp_constr (Location.mknoloc (Longident.Lident (String.make 1 p)), [])
  | Conj (p, q) -> mkcoretype @@ Ptyp_tuple (List.map of_prop [p; q])
  | Disj (p, q) -> mkcoretype @@
    Ptyp_constr (Location.mknoloc (Longident.Lident "either"),
    List.map of_prop [p; q])
  | Imp (p, q) -> mkcoretype @@ Ptyp_arrow (Nolabel, of_prop p, of_prop q)
  | False -> mkcoretype @@
    Ptyp_constr (Location.mknoloc (Longident.Lident "false"), [])
  | Forall _ | Exists _ | Pred _ -> failwith "unsupported predicate logic"

let mkexpr pexp_desc = {
  pexp_desc;
  pexp_loc = Location.none;
  pexp_attributes = [];
}

let mkpat ppat_desc = {
  ppat_desc;
  ppat_loc = Location.none;
  ppat_attributes = [];
}

let mkconstr id expr =
  mkexpr @@ Pexp_construct (Location.mknoloc id, Some expr)

let rec of_expr = function
  | Expr.Var x -> mkexpr @@ Pexp_ident (Location.mknoloc @@ Longident.Lident x)
  | Cons (t, t') -> mkexpr @@ Pexp_tuple (List.map of_expr [t; t'])
  | Let (l, r, t, b) ->
    let binding = {
      pvb_expr = of_expr t;
      pvb_attributes = [];
      pvb_loc = Location.none;
      pvb_pat =
        mkpat @@ Ppat_tuple (
          [l; r] |>
          List.map (fun x -> mkpat @@ Ppat_var (Location.mknoloc x))
        )
    } in
    mkexpr @@ Pexp_let (Nonrecursive, [binding], of_expr b)
  | Abs (x, prop, t) ->
    mkexpr @@ Pexp_fun (
      Nolabel,
      None,
      mkpat @@ Ppat_constraint (
        mkpat @@ Ppat_var (Location.mknoloc x),
        of_prop prop
      ),
      of_expr t
    )
  | App (f, x) ->
    (* FIXME? *)
    mkexpr @@ Pexp_apply (of_expr f, [Nolabel, of_expr x])
  | Left t ->
    mkconstr (Longident.Lident "Left") (of_expr t)
  | Right t ->
    mkconstr (Longident.Lident "Right") (of_expr t)
  | Match (d, l, lt, r, rt) ->
    let mkcase (n, x, t) = {
      pc_lhs = mkpat @@ Ppat_construct (
        Location.mknoloc (Longident.Lident n),
        Some (mkpat @@ Ppat_var (Location.mknoloc x))
      );
      pc_guard = None;
      pc_rhs = of_expr t;
    } in
    mkexpr @@ Pexp_match (of_expr d, List.map mkcase [
      "Left", l, lt;
      "Right", r, rt
    ])
  | ExFalso t ->
    mkexpr @@ Pexp_match (of_expr t, [{
      pc_lhs = mkpat Ppat_any;
      pc_guard = None;
      pc_rhs = mkexpr Pexp_unreachable;
    }])
