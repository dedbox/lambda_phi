open Syntax

let phi : phi -> string = function
  | Future -> "+"
  | Past -> "-"
  | Present -> assert false

let rec exp : exp -> string =
  let rec shift_exp : exp -> string = function
    | Shift ((Shift _ as e), p) -> shift_exp e ^ phi p
    | Shift (((App _ | Lam _) as e), p) -> "(" ^ exp e ^ ")^" ^ phi p
    | Shift (((Num _ | Var _) as e), p) -> exp e ^ "^" ^ phi p
    | Num _ | Var _ | App _ | Lam _ -> assert false
  in
  let lhs_exp : exp -> string = function
    | (Num _ | Var _ | App _) as e -> exp e
    | Shift _ as e -> shift_exp e
    | Lam _ as e -> "(" ^ exp e ^ ")"
  in
  let rhs_exp : exp -> string = function
    | (Num _ | Var _ | Lam _) as e -> exp e
    | Shift _ as e -> shift_exp e
    | App _ as e -> "(" ^ exp e ^ ")"
  in
  function
  | Var x -> x
  | Num n -> Int.to_string n
  | Shift _ as e -> shift_exp e
  | App (e1, e2) -> lhs_exp e1 ^ " " ^ rhs_exp e2
  | Lam (Present, x, e) -> "\\" ^ x ^ "." ^ exp e
  | Lam (p, x, e) -> "\\" ^ phi p ^ x ^ "." ^ exp e

let print : exp -> string = exp

let%test_module _ =
  ( module struct
    open OUnit2

    let assert_print_equal (want : string) (got : exp) : unit =
      assert_equal ~printer:(fun x -> x) want (print got)

    let%test_unit _ = assert_print_equal "123" (num 123)

    let%test_unit _ = assert_print_equal "-123" (num (-123))

    let%test_unit _ = assert_print_equal "x" (var "x")

    let%test_unit _ = assert_print_equal "\\y.z" (lam Present "y" (var "z"))

    let%test_unit _ = assert_print_equal "\\y.123" (lam Present "y" (num 123))

    let%test_unit _ = assert_print_equal "\\+y.z" (lam Future "y" (var "z"))

    let%test_unit _ = assert_print_equal "\\+y.456" (lam Future "y" (num 456))

    let%test_unit _ = assert_print_equal "\\-y.z" (lam Past "y" (var "z"))

    let%test_unit _ = assert_print_equal "\\-y.-789" (lam Past "y" (num (-789)))

    let%test_unit _ = assert_print_equal "f x" (app (var "f") (var "x"))

    let%test_unit _ = assert_print_equal "f 12" (app (var "f") (num 12))

    let%test_unit _ = assert_print_equal "g -34" (app (var "g") (num (-34)))

    let%test_unit _ =
      assert_print_equal "\\h.h 56" (lam Present "h" (app (var "h") (num 56)))

    let%test_unit _ =
      assert_print_equal "i j k" (app (app (var "i") (var "j")) (var "k"))

    let%test_unit _ =
      assert_print_equal "l m n o"
        (app (app (app (var "l") (var "m")) (var "n")) (var "o"))

    let%test_unit _ =
      assert_print_equal "p (q r) s"
        (app (app (var "p") (app (var "q") (var "r"))) (var "s"))

    let%test_unit _ =
      assert_print_equal "(\\t.t) 78" (app (lam Present "t" (var "t")) (num 78))

    let%test_unit _ =
      assert_print_equal "\\+u.\\-v.u v"
        (lam Future "u" (lam Past "v" (app (var "u") (var "v"))))

    let%test_unit _ =
      assert_print_equal "\\+u.\\-v.u v"
        (lam Future "u" (lam Past "v" (app (var "u") (var "v"))))

    let%test_unit _ =
      assert_print_equal "(\\+w.\\-x.w x) 90"
        (app (lam Future "w" (lam Past "x" (app (var "w") (var "x")))) (num 90))
  end )
