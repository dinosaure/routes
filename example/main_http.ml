open Rock
open Lwt.Infix

module Uppercase = struct
  let m =
    let filter handler req =
      handler req
      >>= fun resp ->
      Body.to_string resp.Response.body
      >|= fun body -> { resp with body = Body.of_string (String.uppercase_ascii body) }
    in
    Rock.Middleware.create ~filter ~name:"Uppercaser"
  ;;
end

let spec =
  Routes_http.make_spec
    ~middlewares:[ Uppercase.m ]
    ~methods:[ `GET ]
    ~target:Routes.(s "hello" /? nil)
    (fun _req -> Lwt.return @@ Response.make ~body:(Body.of_string "Hello World") ())
;;

let () =
  print_endline @@ Format.asprintf "%a" Routes_http.pp_spec spec;
  let listen_addr = Unix.(ADDR_INET (inet_addr_loopback, 8080)) in
  Routes_http.run spec listen_addr
;;
