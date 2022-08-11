open Lwt
open Cohttp
open Cohttp_lwt_unix

let reqBody = 
  let uri = Uri.of_string "localhost:8080/version" in
  Client.call `GET uri >>= fun (_resp, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body -> body

let () =
  let respBody = Lwt_main.run reqBody in
  print_endline (respBody)