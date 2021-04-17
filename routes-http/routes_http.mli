type spec

val make_spec
  :  ?methods:Httpaf.Method.t list
  -> ?middlewares:Rock.Middleware.t list
  -> target:('a, Rock.Handler.t) Routes.target
  -> 'a
  -> spec

val pp_spec : Format.formatter -> spec -> unit

val run : spec -> Unix.sockaddr -> 'a
