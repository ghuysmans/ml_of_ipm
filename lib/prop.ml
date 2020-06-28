type t =
  | Prop of char
  | Conj of t * t
  | Disj of t * t
  | Imp of t * t
  | False

let rec pp ppf = function
  | Prop p -> Format.fprintf ppf "%c" p
  | Conj (p, q) -> Format.fprintf ppf "@[<hov>(%a /\\@ %a)@]" pp p pp q
  | Disj (p, q) -> Format.fprintf ppf "@[<hov>(%a \\/@ %a)@]" pp p pp q
  | Imp (p, q) -> Format.fprintf ppf "@[<hov>(%a ->@ %a)@]" pp p pp q
  | False -> Format.fprintf ppf "False"
