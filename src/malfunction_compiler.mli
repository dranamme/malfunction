type outfiles
val delete_temps : outfiles -> unit

type options = [`Verbose | `Shared | `ForPack of string | `Package of string | `Dontlink of string | `Linkpkg | `Thread | `Optimize | `Bytecode | `Async] list


val to_lambda : Env.t -> Malfunction.t -> Lambda.lambda

val to_lambda_tmca : Env.t -> Malfunction.t -> Lambda.lambda Picos.Computation.t
val to_lambda_async : Env.t -> Malfunction.t -> Lambda.lambda Picos.Computation.t

val compile_module :
  ?options:options ->
  filename:string ->
  Malfunction_parser.moduleexp ->
  outfiles

val compile_cmx : ?options:options -> string -> outfiles
val compile_cmo : ?options:options -> string -> outfiles

val link_executable : ?options:options -> string -> outfiles -> int

val compile_and_load : ?options:options -> Malfunction.t -> Obj.t
