open Parsetree

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
  | Abs (x, t) -> mkexpr @@
    Pexp_fun (Nolabel, None, mkpat (Ppat_var (Location.mknoloc x)), of_expr t)
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
