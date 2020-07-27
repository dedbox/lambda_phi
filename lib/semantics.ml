(* open Syntax
 * 
 * module Bindings = Map.Make (struct
 *   type t = string * int [@@deriving ord]
 * end)
 * 
 * let dn (bindings : exp Bindings.t) (e : exp) : exp Bindings.t * exp =
 *   let keys, values = List.split (Bindings.bindings bindings) in
 *   let xs, zs = List.split keys in
 *   let bindings_minus =
 *     Bindings.of_seq
 *       List.(to_seq (combine (combine xs (map (fun z -> z - 1) zs)) values))
 *   in
 *   let e_minus =
 *     match e with
 *     | Shift (e1, Future) -> e1
 *     | Shift (e1, Present) -> shift e1 Past
 *     | Shift (_, Past) | Lam _ | App _ | Var _ -> shift e Past
 *   in
 *   (bindings_minus, e_minus)
 * 
 * let up (bindings : exp Bindings.t) (e : exp) : exp Bindings.t * exp =
 *   let keys, values = List.split (Bindings.bindings bindings) in
 *   let xs, zs = List.split keys in
 *   let bindings_plus =
 *     Bindings.of_seq
 *       List.(to_seq (combine (combine xs (map (fun z -> z + 1) zs)) values))
 *   in
 *   let e_plus =
 *     match e with
 *     | Shift (e1, Past) -> e1
 *     | Shift (e1, Present) -> shift e1 Future
 *     | Shift (_, Future) | Lam _ | App _ | Var _ -> shift e Future
 *   in
 *   (bindings_plus, e_plus)
 * 
 * let rec step (sigma : exp Bindings.t) : exp -> (exp Bindings.t * exp) option =
 *   function
 *   | Shift (e1_plus, Future) ->
 *       let sigma_minus, e1 = dn sigma e1_plus in
 *       let%map.Option sigma_minus', e1' = step sigma_minus e1 in
 *       up sigma_minus' e1'
 *   | Shift (e1, Present) -> step sigma e1
 *   | Lam _ -> None
 *   | App (e1, e2) -> (
 *       match (is_value e1, is_value e2) with
 *       | false, _ ->
 *           let%map.Option sigma', e1' = step sigma e1 in
 *           (sigma', App (e1', e2))
 *       | true, false ->
 *           let%map.Option sigma', e2' = step sigma e2 in
 *           (sigma', App (e1, e2'))
 *       | true, true -> (
 *           match e1 with
 *           | Lam (p, x, e11) ->
 *               let sigma' = Bindings.add (x, phase_num p) e2 sigma in
 *               step sigma' e11
 *           | Shift (_e11, Past) -> failwith __LOC__
 *           | Shift _ | Var _ | App _ -> assert false ) )
 *   | Var x ->
 *       let%map.Option e = Bindings.find_opt (x, 0) sigma in
 *       (sigma, e)
 *   | Shift (e1_minus, Past) ->
 *       let sigma_plus, e1 = up sigma e1_minus in
 *       let%map.Option sigma_plus', e1' = step sigma_plus e1 in
 *       dn sigma_plus' e1'
 * 
 * let rec eval ?(sigma : exp Bindings.t = Bindings.empty) (e : exp) : exp option =
 *   match step sigma e with
 *   | Some (sigma', e') -> eval ~sigma:sigma' e'
 *   | None -> if is_value e then Some e else None
 * 
 * let%test _ = eval (lam Present "x" (Var "x")) = Some (lam Present "x" (Var "x")) *)
