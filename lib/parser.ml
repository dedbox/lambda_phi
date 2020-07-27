open Angstrom
open Syntax

let whitespace =
  skip_while (function ' ' | '\t' | '\r' | '\n' -> true | _ -> false)

let phi : phi t =
  option ' ' (char '+' <|> char '-') >>= function
  | '+' -> return Future
  | '-' -> return Past
  | _ -> fail "not a phase shift"

let exp : exp t =
  fix (fun exp ->
      let var_exp : exp t =
        let x : string t =
          take_while1 (fun c ->
              let i = int_of_char c in
              i >= int_of_char 'a' && i <= int_of_char 'z')
        in
        var <$> x
      in

      let num_exp : exp t =
        let s : string t =
          ( ^ )
          <$> option "" (string "-")
          <*> take_while1 (fun c ->
                  let i = int_of_char c in
                  i >= int_of_char '0' && i <= int_of_char '9')
        in
        let n : int t = int_of_string <$> s in
        num <$> n
      in

      let lam_exp : exp t =
        let p : phi t = char '\\' *> option Present phi in
        let x : string t =
          var_exp >>= function Var x -> return x | _ -> assert false
        in
        let e : exp t = char '.' *> exp in
        lam <$> p <*> x <*> e
      in

      let parenthesized_exp : exp t =
        char '(' *> exp <* whitespace <* char ')'
      in

      let shift_exp : exp t =
        let e = num_exp <|> var_exp <|> parenthesized_exp in
        let ps = char '^' *> many1 phi in
        List.fold_left shift <$> e <*> ps
      in

      let nonapp_exp : exp t =
        shift_exp <|> num_exp <|> var_exp <|> parenthesized_exp <|> lam_exp
      in

      let app_exp : exp t =
        List.fold_left app <$> nonapp_exp <*> many1 (whitespace *> nonapp_exp)
      in

      whitespace
      *> ( lam_exp <|> app_exp <|> shift_exp <|> num_exp <|> var_exp
         <|> parenthesized_exp ))

let parse (str : string) : exp =
  match parse_string ~consume:All (exp <* whitespace <* end_of_input) str with
  | Ok e -> e
  | Error msg -> failwith msg

let%test_module _ =
  ( module struct
    open OUnit2

    let assert_parse_equal (want : exp) (got : string) : unit =
      assert_equal ~printer:Syntax.show_exp want (parse got)

    let%test_unit _ = assert_parse_equal (num 123) "123"

    let%test_unit _ = assert_parse_equal (num (-123)) "-123"

    let%test_unit _ = assert_parse_equal (var "x") "x"

    let%test_unit _ = assert_parse_equal (lam Present "y" (var "z")) "\\y.z"

    let%test_unit _ = assert_parse_equal (lam Present "y" (num 123)) "\\y.123"

    let%test_unit _ = assert_parse_equal (lam Future "y" (var "z")) "\\+y.z"

    let%test_unit _ = assert_parse_equal (lam Future "y" (num 456)) "\\+y.456"

    let%test_unit _ = assert_parse_equal (lam Past "y" (var "z")) "\\-y.z"

    let%test_unit _ = assert_parse_equal (lam Past "y" (num (-789))) "\\-y.-789"

    let%test_unit _ = assert_parse_equal (app (var "f") (var "x")) "f x"

    let%test_unit _ = assert_parse_equal (app (var "g") (num (-34))) "g -34"

    let%test_unit _ =
      assert_parse_equal (lam Present "h" (app (var "h") (num 56))) "\\h.h 56"

    let%test_unit _ =
      assert_parse_equal (app (app (var "i") (var "j")) (var "k")) "i j k"

    let%test_unit _ =
      assert_parse_equal
        (app (app (app (var "l") (var "m")) (var "n")) (var "o"))
        "l m n o"

    let%test_unit _ =
      assert_parse_equal
        (app (app (var "p") (app (var "q") (var "r"))) (var "s"))
        "p (q r) s"

    let%test_unit _ =
      assert_parse_equal (app (lam Present "t" (var "t")) (num 78)) "(\\t.t) 78"

    let%test_unit _ =
      assert_parse_equal
        (lam Future "u" (lam Past "v" (app (var "u") (var "v"))))
        "\\+u.\\-v.u v"

    let%test_unit _ =
      assert_parse_equal
        (lam Future "u" (lam Past "v" (app (var "u") (var "v"))))
        "\\+u.\\-v.u v"

    let%test_unit _ =
      assert_parse_equal
        (app (lam Future "w" (lam Past "x" (app (var "w") (var "x")))) (num 90))
        "(\\+w.\\-x.w x) 90"

    let%test_unit _ = assert_parse_equal (shift (num 1) Future) "1^+"

    let%test_unit _ = assert_parse_equal (shift (num 2) Past) "2^-"

    let%test_unit _ =
      assert_parse_equal (shift (shift (num 2) Past) Past) "2^--"

    let%test_unit _ =
      assert_parse_equal
        (shift (shift (shift (num 3) Future) Past) Past)
        "3^+--"

    let%test_unit _ = assert_parse_equal (shift (num (-4)) Future) "-4^+"

    let%test_unit _ = assert_parse_equal (shift (num (-5)) Past) "-5^-"

    let%test_unit _ = assert_parse_equal (shift (var "x") Future) "x^+"

    let%test_unit _ =
      assert_parse_equal (shift (app (var "f") (var "x")) Past) "(f x)^-"

    let%test_unit _ =
      assert_parse_equal
        (shift (app (app (var "f") (var "x")) (var "y")) Future)
        "(f x y)^+"

    let%test_unit _ =
      assert_parse_equal (shift (lam Present "x" (var "x")) Future) "(\\x.x)^+"

    let%test_unit _ =
      assert_parse_equal
        (shift (app (lam Future "y" (shift (var "y") Past)) (num 9)) Past)
        "((\\+y.y^-) 9)^-"

    let%test_unit _ =
      assert_parse_equal (app (shift (num 1) Past) (num 2)) "1^- 2"

    let%test_unit _ =
      assert_parse_equal (app (num 3) (shift (num 4) Future)) "3 4^+"

    let%test_unit _ =
      assert_parse_equal
        (app (shift (num 5) Future) (shift (num 6) Past))
        "5^+ 6^-"
  end )
