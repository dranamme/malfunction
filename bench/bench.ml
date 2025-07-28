open Malfunction
open Malfunction_compiler
(* open Picos *)
(* open Bechamel *)

open QCheck2


let n_domains = Domain.recommended_domain_count () (* Degree of parallelism *)
let seed = 1312 (* Seed for random tree gen. *)
let size = 100000 (* Expr size *)

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

let vecnew len def = Mvecnew(`Array, len, def)

let switch e l = Mswitch (e, l)

let num n = Mnum (`Int n)


let tag n = `Tag n



let gen_prog_qcheck size =
  size |> Gen.(fix (fun self n ->
                   match n with
                   | 0 -> num <$> nat
                   | n ->
                      frequency
                        [3, num <$> nat;
                         5, map3 numop2
                              (oneofl [`Add; `Sub; `Mul; `Div; `Mod])
                              (self (n/2))
                              (self (n/2));
                         1, map2 vecnew (self (n/2)) (self (n/2));
                         4, map2 switch (self (n/2))
                              (list @@ pair (list (tag <$> nat)) (self (n/2)))
                        ]
          ))





let _ =
  to_lambda_tmca Env.empty (Gen.generate1 (gen_prog_qcheck size))


(* let gen_vector_op size : t = *)
(*   match Random.int with *)
(*   | *)


(* let gen_prog () : t = *)
(*   Mnum (`Int 3) *)
