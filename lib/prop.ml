type t =
  | Prop of char
  | Conj of t * t
  | Disj of t * t
  | Imp of t * t
  | False
