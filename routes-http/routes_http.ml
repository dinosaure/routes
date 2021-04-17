type spec =
  { methods : Httpaf.Method.t list option
  ; middlewares : Rock.Middleware.t list
  ; route : Rock.App.t Routes.route
  }

let make_spec ?methods ?(middlewares = []) ~target handler =
  let route = Routes.(target @--> handler) in
  let route =
    Routes.map
      (fun handler ->
        let handler req =
          match methods with
          | None | Some [] -> handler req
          | Some xs ->
            let meth = req.Rock.Request.meth in
            if List.mem meth xs
            then handler req
            else
              Lwt.return
                (Rock.Response.make
                   ~status:`Method_not_allowed
                   ~body:
                     (Rock.Body.of_string
                        (Httpaf.Status.default_reason_phrase `Method_not_allowed))
                   ())
        in
        Rock.App.create ~middlewares ~handler ())
      route
  in
  { methods; middlewares; route }
;;

let app spec =
  let handler req =
    let target = req.Rock.Request.target in
    match Routes.match' (Routes.one_of [ spec.route ]) ~target with
    | None ->
      Lwt.return
        (Rock.Response.make
           ~status:`Not_found
           ~body:(Rock.Body.of_string "Page not found")
           ())
    | Some app ->
      let filters =
        List.map (fun m -> m.Rock.Middleware.filter) app.Rock.App.middlewares
      in
      let service = Rock.Filter.apply_all filters app.Rock.App.handler in
      service req
  in
  Rock.App.create ~handler ()
;;

let pp_spec fmt spec =
  let m_msg =
    match spec.methods with
    | None | Some [] -> "No constraint on HTTP verb"
    | Some xs ->
      let xs = List.map Httpaf.Method.to_string xs in
      Printf.sprintf "Matches HTTP Verbs: %s" @@ String.concat ", " xs
  in
  let middlewares =
    match spec.middlewares with
    | [] -> "No middlewares used"
    | xs ->
      Printf.sprintf
        "Middlewares: %s"
        (String.concat ", " @@ List.map (fun m -> m.Rock.Middleware.name) xs)
  in
  Format.fprintf fmt "Spec: %a\n%s\n%s" Routes.pp_route spec.route m_msg middlewares
;;

let run spec listen_address =
  let open Lwt.Infix in
  let connection_handler addr fd =
    let f ~request_handler ~error_handler =
      Httpaf_lwt_unix.Server.create_connection_handler
        ~request_handler:(fun _ -> request_handler)
        ~error_handler:(fun _ -> error_handler)
        addr
        fd
    in
    Rock.Server_connection.run f (app spec)
  in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket listen_address connection_handler
      >>= fun _ -> Lwt.return_unit);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
;;
