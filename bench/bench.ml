open Malfunction
open Malfunction_compiler
(* open Picos *)
(* open Bechamel *)

open QCheck2


let n_domains = Domain.recommended_domain_count () (* Degree of parallelism *)
let seed = 1312 (* Seed for random tree gen. *)
let size = 1000000 (* Expr size *)

(* let () = *)
(*   Fmt.pr "Configuration: %d domains, %d seed, %d tree size@." *)
(*     n_domains seed size *)

let () = Random.init seed


let gen_vector_type () =
  match Random.int 2 with
  | 0 -> `Array
  | _ -> `Bytevec

let rec gen_int_expr size : t =
  if size = 0 then
    Mnum (`Int (Random.int_in_range ~min:(-42000) ~max:42000))
  else
    let op = match Random.int 5 with
      | 0 -> `Add
      | 1 -> `Sub
      | 2 -> `Mul
      | 3 -> `Div
      | _ -> `Mod
    in
    let left_size = Random.int size in
    Mnumop2 (op, `Int, gen_int_expr left_size, gen_int_expr (size - left_size - 1))




let numop2 op l r = Mnumop2(op, `Int, l, r)

let switch e l = Mswitch (e, l)

let num n = Mnum (`Int n)




let gen_prog_qchek size =
  size |> Gen.(fix (fun self n ->
                   match n with
                   | 0 -> map num nat
                   | n ->
                      frequency
                        [1, map num nat;
                         2, map3 numop2
                              (oneofl [`Add; `Sub; `Mul; `Div; `Mod])
                              (self (n/2))
                              (self (n/2))]
          ))





let _ =
  to_lambda Env.empty (gen_int_expr size)


(* let gen_vector_op size : t = *)
(*   match Random.int with *)
(*   | *)


(* let gen_prog () : t = *)
(*   Mnum (`Int 3) *)
