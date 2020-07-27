type phi = Past | Present | Future [@@deriving ord, show]

let int_of_phi : phi -> int = function Past -> -1 | Present -> 0 | Future -> 1

let phi_of_int : int -> phi = function
  | 0 -> Present
  | i -> if i < 0 then Past else Future

type exp =
  | Lam of phi * string * exp
  | Shift of exp * phi
  | App of exp * exp
  | Var of string
  | Num of int
[@@deriving show, variants]

let is_value : exp -> bool = function
  | Lam _ | Shift (_, Past) | Num _ -> true
  | Shift _ | App _ | Var _ -> false
