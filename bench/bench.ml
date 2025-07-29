open Malfunction
open Malfunction_compiler
(* open Picos *)
(* open Bechamel *)

open QCheck2


let n_domains = 4 (* Degree of parallelism *)
let seed = 1312 (* Seed for random tree gen. *)

let () =
  Fmt.pr "Configuration: %d domains, %d seed@."
    n_domains seed

let numop2 op l r = Mnumop2(op, `Int, l, r)

let vecnew len def = Mvecnew(`Array, len, def)

let veclen len = Mveclen (`Array, len)

let switch e l = Mswitch (e, l)

let num n = Mnum (`Int n)


let tag n = `Tag n



let gen_sum ?(bound=max_int) n =
  Gen.(if n <= 1 then pure (0, 0)
       else let+ k = (int_bound @@ min bound (n-1)) in (k, n-k))



let gen_prog_qcheck =
  let open Gen in
  fix @@ fun self n ->
  if n <= 1 then
    num <$> nat
  else
    frequency
      [1, veclen <$> (self (n-1));
       4, n-1 |> gen_sum >>= (fun (k, nk) ->
           map3 numop2
             (oneofl [`Add; `Sub; `Mul; `Div; `Mod])
             (self k)
             (self nk));
       2, n-1 |> gen_sum >>= (fun (k, nk) ->
           map2 vecnew (self k) (self nk));
       20, (let* k, nk = gen_sum ~bound:10 (n-1) in
           let* t = self k in
           let* t' = self nk in
           pure @@ Mlet ([`Unnamed t], t')
          );
       2, n-1 |> gen_sum >>= (fun (k, nk) ->
           map2 switch (self k)
             ((1 -- 10) >>= fun len ->
              list_size (pure len) @@
              pair
                (list_size (pure 1) (tag <$> nat))
                (self (nk/len))))
      ]

let rec size_of_term t =
  match t with
  | Mvar _ | Mnum _ | Mstring _ | Mglobal _ -> 1
  | Mnumop1 (_,_, e) | Mconvert (_, _, e) | Mlazy e | Mforce e
    | Mfield (_ ,e) | Mveclen (_, e) | Mlambda (_, e) -> size_of_term e + 1
  | Mnumop2 (_, _, e1, e2) | Mvecnew (_, e1, e2) | Mvecget (_, e1, e2) -> 1 + size_of_term e1 + size_of_term e2
  | Mlet (l, t) -> 1 + size_of_binding_list l + size_of_term t
  | Mapply (t, l) -> List.fold_left (fun i t -> i + size_of_term t) (1 + size_of_term t) l
  | Mswitch (t, l) ->  List.fold_left (fun i (_,t) -> i + size_of_term t) (1 + (size_of_term t)) l
  | Mblock (_, l) -> List.fold_left (fun i t -> i + size_of_term t) 1 l
  | Mvecset (_, e1, e2, e3) -> 1 + size_of_term e1 + size_of_term e2 + size_of_term e3


and size_of_binding_list l =
  List.fold_left (fun i b -> match b with
                  | `Unnamed e | `Named (_, e) -> size_of_term e + i
                  | `Recursive l ->
                     List.fold_left (fun i (_,t) -> i + size_of_term t) i l) 0 l


let rand = Random.State.make [| seed |]

(* range from 10¹ to 10⁶ elements, 5 points per order. *)
let args = List.init 26 (fun x -> int_of_float (10. ** (float x/.5. +. 1.)))
let trees =
  List.map (fun i -> i, Gen.generate1 ~rand @@ gen_prog_qcheck i) args
  |> List.to_seq
  |> Hashtbl.of_seq

let () =    
  Format.printf "@[<v2>Sizes : {@ ";
  Seq.iter
    (fun (i, t) -> Format.printf "%i: %i;@," i (size_of_term t))
    (Hashtbl.to_seq trees);
  Format.printf "}@]";
  ()
  
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
