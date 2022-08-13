open Lwt
open Cohttp
open Cohttp_lwt_unix

(* Request using Cohttp *)

let body =
  Client.get (Uri.of_string "http://localhost:8080/version") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

(* let post_body =
  Client.post (Uri.of_string "http://localhost:8080/retrieve_job_description_from_user") *)

let () =
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body)