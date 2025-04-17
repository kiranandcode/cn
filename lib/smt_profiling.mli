

val close_profiler : unit -> unit

val init_profiler : string -> unit

val with_profiling: ?hash:(unit -> string) -> ('b -> 'a) -> 'b -> 'a
