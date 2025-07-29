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

let veclen len = Mveclen (`Array, len)

let switch e l = Mswitch (e, l)

let num n = Mnum (`Int n)


let tag n = `Tag n



let gen_sum n =
  Gen.(if n <= 0 then pure (0, 0)
       else let+ k = (int_bound (n-1)) in (k, n-k))



let gen_prog_qcheck size =
  size |> Gen.(fix (fun self n ->
                   match n with
                   | 0 -> num <$> nat
                   | n ->
                      frequency
                        [1, num <$> nat;
                         1, veclen <$> (self (n-1));
                         3, n-1 |> gen_sum >>= (fun (k, nk) ->
                           map3 numop2
                             (oneofl [`Add; `Sub; `Mul; `Div; `Mod])
                             (self k)
                             (self nk));
                         1, n-1 |> gen_sum >>= (fun (k, nk) ->
                             map2 vecnew (self k) (self nk));
                         4, (let* k = 1 -- max n 10 in
                              let* t = self k in
                              let* t' = self (n-k-1) in
                              pure @@ Mlet ([`Unnamed t], t')
                             );
                         2, n-1 |> gen_sum >>= (fun (k, nk) ->
                           map2 switch (self k)
                             ((1 -- 1312) >>= (fun k -> list_size (pure k) @@ pair (list_size (1--7)
                                                                                   (tag <$> nat)) (self (nk/k)))))
                        ]
          ))





(* let _ = *)
(*   to_lambda_tmca Env.empty (Gen.generate1 (gen_prog_qcheck size)) *)


(* let gen_vector_op size : t = *)
(*   match Random.int with *)
(*   | *)


(* let gen_prog () : t = *)
(*   Mnum (`Int 3) *)

let rand = Random.State.make [| seed |]

(* range from 10¹ to 10⁴ elements, 5 points per order. *)
let args = List.init 16 (fun x -> int_of_float (10. ** (float x/.5. +. 1.)))
let trees =
  List.map (fun i -> i, Gen.generate1 ~rand @@ gen_prog_qcheck i) args
  |> List.to_seq
  |> Hashtbl.of_seq

open Picos
open Bechamel


let bench_tree_sync name f =
  Test.make_indexed ~name ~fmt:"%s %d"
    ~args
    (fun size -> Staged.stage @@ fun () ->
      f @@ Hashtbl.find trees size)

let bench_tree_async name f =
  Test.make_indexed ~name ~fmt:"%s %d"
    ~args
    (fun size ->
       Staged.stage @@ fun () ->
       Computation.await @@ f @@ Hashtbl.find trees size)

let indexed_benchs =
  Compmisc.init_path ();
  let test_sync = bench_tree_sync "to_lambda sync" (to_lambda (Compmisc.initial_env ())) in
  let test_async = bench_tree_async "to_lambda async" (to_lambda_async (Compmisc.initial_env ())) in
  let test_tmca = bench_tree_async "to_lambda tmca" (to_lambda_tmca (Compmisc.initial_env ())) in
  Test.make_grouped ~name:"tree bench" ~fmt:"%s %s"
    [ test_sync;
      test_async;
      test_tmca;
    ]


(** Bechamel boilerplate *)
let run_benchmark file benchs =
  let open Bechamel in
  let instances = [ Toolkit.Instance.monotonic_clock ] in
  let cfg =
    Benchmark.cfg ~stabilize:true ~limit:1000 ~quota:(Time.second 10.) ()
  in
  let raw_results =
    Benchmark.all cfg instances benchs
  in
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:[| Measure.run |]
  in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  let dst = Bechamel_js.channel file in
  let nothing _ = Ok () in
  let results =
    Bechamel_js.emit ~dst nothing ~compare ~x_label:Measure.run
      ~y_label:(Measure.label Toolkit.Instance.monotonic_clock)
      (results, raw_results)
  in
  match results with Ok () -> () | Error (`Msg err) -> invalid_arg err

let main () =
  run_benchmark "malfunction.raw.json" indexed_benchs;
  ()

let () =
  let pool = Moonpool.Ws_pool.create ~num_threads:n_domains () in
  Moonpool.Runner.run_wait_block pool main
