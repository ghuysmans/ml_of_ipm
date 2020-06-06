open Coc

let () = Format.printf "@[<v>"
let aia = EAbs ("A", TProp, EAbs ("h", TVar "A", EVar "h"))
(* let t = TProd ("A", TProp, TProd ("h", TVar "A", TVar "A")) *)
let () = Format.printf "%a@;" (pp_fr []) aia
let abia = EAbs ("A", TProp, EAbs ("B", TProp, EAbs ("h", TVar "A", EVar "h")))
let () = Format.printf "%a@;" (pp_fr []) abia
let tr =
  EAbs ("A", TProp, EAbs ("B", TProp, EAbs ("C", TProp,
    EAbs ("f", TProd ("h", TVar "A", TVar "B"),
    EAbs ("g", TProd ("h", TVar "B", TVar "C"),
    EAbs ("a", TVar "A", EApp (EVar "g", EApp (EVar "f", EVar "a"))))))))
let () = Format.printf "%a@;" (pp_fr []) tr
let () = Format.printf "@]"
