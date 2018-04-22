open Core
open Async_kernel
open Cohttp_async

let split_path path = 
  let non_empty s = not (String.is_empty s) in 
  String.split ~on:'/' path |> List.filter ~f:non_empty

let () = assert (split_path "/u/12487/" = ["u"; "12487"])

let handler ~body:_ _sock req =
  let uri = Cohttp.Request.uri req in
  match split_path (Uri.path uri) with
  | ["u";] ->
    "uploading anonymously" |> Server.respond_string
  | ["u"; name] ->
    "uploading " ^ name |> Server.respond_string
  | ["d"; id] -> 
    "downloading " ^ id |> Server.respond_string
  | _ -> 
    Server.respond_string ~status:`Not_found "Route not found"

let start_server port () =
  Printf.eprintf "Listening for HTTP on port %d\n" port;
  Printf.eprintf "Try 'curl http://localhost:%d/test?hello=xyz'\n%!" port;
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Async_extra.Tcp.Where_to_listen.of_port port) handler
  >>= fun _ -> Deferred.never ()

let run () =
  let module Command = Async_extra.Command in
  Command.async_spec
    ~summary:"Start a hello world Async server"
    Command.Spec.(
      empty +>
      flag "-p" (optional_with_default 8080 int)
        ~doc:"int Source port to listen on"
    ) start_server

  |> Command.run
