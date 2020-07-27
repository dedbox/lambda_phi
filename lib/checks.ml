open QCheck
open Syntax

let rec shrink : exp Shrink.t =
  let open Iter in
  function
  | App (e1, e2) -> map2 app (shrink e1) (shrink e2)
  | Lam (p, x, e) -> map (lam p (String.sub x 0 1)) (shrink e)
  | Shift (Shift (e, Future), Past) -> return e <+> shrink e
  | Shift (Shift (e, Past), Future) -> return e <+> shrink e
  | Shift (e, p) -> map (fun e' -> shift e' p) (shrink e)
  | Var x -> map var (return (String.sub x 0 1))
  | Num _ -> map num (return 0)

let phi2 : phi Gen.t = Gen.oneofl [ Future; Past ]

let phi3 : phi Gen.t = Gen.oneofl [ Future; Present; Past ]

let idchar : char Gen.t = Gen.char_range 'a' 'z'

let id : string Gen.t =
  Gen.(
    String.of_seq
    <$> (List.to_seq <$> (List.cons <$> idchar <*> list_size small_nat idchar)))

let exp : exp arbitrary =
  make ~print:Printer.print ~shrink
    Gen.(
      sized
      @@ fix (fun exp n ->
             let app_exp : int -> exp t =
               fix (fun app_exp m ->
                   match m with
                   | 0 -> var <$> id
                   | _ -> app <$> app_exp (m / 2) <*> exp (m / 2))
             in
             match n with
             | 0 -> oneof [ num <$> nat; var <$> id ]
             | _ ->
                 oneof
                   [
                     shift <$> exp (n - 1) <*> phi2;
                     lam <$> phi3 <*> id <*> exp (n - 1);
                     app <$> app_exp (n / 2) <*> exp (n / 2);
                   ]))

let parse_print_id (e : exp) : bool =
  let e' = Parser.parse (Printer.print e) in
  e' = Parser.parse (Printer.print e)

let test : Test.t = Test.make ~name:"parse_print_id" exp parse_print_id

let%test_unit _ =
  match QCheck_ounit.(run ~argv:[| "-v"; "-bt" |] (to_ounit_test test)) with
  | 0 -> ()
  | 1 -> OUnit.assert_failure "qcheck failed"
  | _ -> assert false
