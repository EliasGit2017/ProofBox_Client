open Data_types
open Bcrypt
open Utils
open Lwt.Infix
open EzAPI
open EzAPI.TYPES
open EzWs

let api_port = ref 8080
let user1_login = "test_user60"
let user1_password = "dummydedada1234!"
let user1_info = "This user is also here for testing purposes"

(* lwt management *)
let waiter, finalizer = Lwt.wait ()
let waiting = ref false
let nrequests = ref 0
let begin_request () = incr nrequests

let end_request () =
  decr nrequests ;
  if !waiting && !nrequests = 0 then Lwt.wakeup finalizer ()
(* // *)

module SessionArg = struct
  type user_id = string
  type user_info = Data_types.user_info

  let user_id_encoding = Json_encoding.string
  let user_info_encoding = Encoding.user_info
  let rpc_path = []
  let token_kind = `CSRF "X-Csrf-Token" (* `Cookie "EZSESSION" *)
end

module Session = EzSessionClient.Make (SessionArg)
open EzAPI.TYPES

let error test n =
  Printf.eprintf "Error: request %s returned code %d\n%!" test n ;
  exit 2

(* For websockets *)
let error2 content = Lwt.return @@ EzDebug.printf "client error %s" content

let basic api =
  begin_request () ;
  EzRequest.ANY.get0 ~msg:"simplest req possible" api Services.version
    ~error:(error "even on the dummiest") (function
    | Ok r ->
        Printf.eprintf "Result for Simple req : %s\n%!"
          (version_test_to_string r) ;
        (* handle_response r *)
        end_request ()
    | Error e ->
        Printf.eprintf "%s\n%!" @@ "very bad error even in basic req" ;
        (* handle_error e *)
        end_request ())

let base_req2 arg api =
  begin_request () ;
  print_endline "sending ==> base_req2" ;
  EzRequest.ANY.post0 ~msg:"Testing simple req with EzRequest.ANY.post0 // "
    ~error:(error "error classic req; no lwt //")
    ~input:arg api Services.version_test_json_body (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e) ;
        end_request ()
    | Ok r ->
        Printf.eprintf "Test base_req2 returned ==> %s\n%!"
          (version_test_to_string r) ;
        end_request ())

let get_jobs arg api =
  begin_request () ;
  EzRequest.ANY.post0 ~msg:"getting jobs from user : "
    ~error:(error "Unable to get jobs |get_jobs| request")
    ~input:arg api Services.sr_job_desc_from_user (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e) ;
        end_request ()
    | Ok r ->
        Printf.eprintf "Jobs : %s\n%!" (job_list_to_string r) ;
        end_request ())

(** Request to send meta_payload : returns all the jobs associated to the client
    who initiated the exchange *)
let send_meta_payload arg api =
  begin_request () ;
  EzRequest.ANY.post0
    ~msg:"writting job in db and retrieving all jobs from user "
    ~error:(error "Unable to send meta_payload ")
    ~input:arg api Services.send_job_metadata (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e) ;
        end_request ()
    | Ok r ->
        (* if list length > 0 ==> make appropriate treatment *)
        Printf.eprintf "Jobs : %s\n%!" (job_list_to_string r) ;
        end_request ())

let get_specific_job arg api =
  begin_request () ;
  EzRequest.ANY.post0 ~msg:"Getting Specific job : "
    ~error:(error "Unable to get specific job [get_specific_job] ")
    ~input:arg api Services.sr_job_desc (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e) ;
        end_request ()
    | Ok r ->
        Printf.eprintf "Job : %s\n%!" (job_list_to_string r) ;
        end_request ())

let test_session arg api =
  let open EzSession.TYPES in
  begin_request () ;
  Session.connect api (function
    | Error _ ->
        Printf.eprintf "Error in Connect \n%!" ;
        exit 2
    | Ok (Some _u) -> assert false
    | Ok None ->
        Session.login api ~login:"test_user60"
          ~password:
            "$2y$08$i2c6b9CeUpLJQs8ZGqmrie399MFdX2w17KPXqiXO4cIY6fnEO1CUS"
          (* TO DEFINE simplify this *) (function
          | Error _ ->
              Printf.eprintf "Error in Login \n%!" ;
              exit 2
          | Ok u ->
              Printf.eprintf
                "auth_login = %s; auth_token = %s; auth_user_id = %s; \
                 auth_user_info = %s\n\
                 %!"
                u.auth_login u.auth_token u.auth_user_id u.auth_user_info ;
              (* Print login info here *)
              EzRequest.ANY.post1 ~msg:"testing service through connect / login"
                api Services.test_session ~error:(error "test session")
                ~input:arg
                ~headers:
                  (("X-Auth-Header2:", "Session-Cookie/Token")
                  :: Session.auth_headers ~token:u.auth_token) "arg-of-post1"
                (function
                | Ok r ->
                    Printf.eprintf "Test test4 returned %s\n%!"
                      (version_test_to_string r) ;
                    Session.logout api ~token:u.auth_token (function
                      | Error _ ->
                          Printf.eprintf "Error in logout\n%!" ;
                          exit 2
                      | Ok bool ->
                          Printf.eprintf "logout OK %b\n%!" bool ;
                          end_request ())
                | Error e ->
                    Printf.eprintf "%s\n%!"
                    @@ Printexc.to_string (proofbox_api_error e) ;
                    end_request ())))

let test_signup arg api =
  begin_request () ;
  EzRequest.ANY.post0 ~msg:"Testing signup service : "
    ~error:(error "Unable to Signup User : [test_signup]")
    ~input:arg api Services.sign_up_new_user (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e) ;
        end_request ()
    | Ok r ->
        Printf.eprintf "\nDefault response ==> %s\n\n%!"
          (Utils.default_server_response_to_string r) ;
        end_request ())

(* **************************************************** *)

let ztest = "/home/elias/OCP/ez_pb_client/example.zip"

let zip_to_str zip_name =
  let zc = open_in_bin zip_name in
  let rec serialize acc =
    match input_char zc with
    | e -> serialize (e :: acc)
    | exception End_of_file -> List.rev acc in
  let res = serialize [] in
  close_in zc;
  String.of_seq (List.to_seq res)
  


(* Websocket client test *)

let client_zt_react _send = function
  | Ok s ->
      EzDebug.printf "client got : { %s } from server" s ;
      Lwt.return_ok ()
  | Error e ->
      EzDebug.printf "client got error from server : %s"
        (Printexc.to_string (Proofbox_api_error e)) ;
      Lwt.return_ok ()

let handle_zt { conn; action = { send; close } } =
  let send_zip =
    send @@ (zip_to_str ztest) >>= function
    | Error _ -> close None
    | Ok () -> Lwt.bind (EzLwtSys.sleep 1.) (fun () -> Lwt.return_ok ()) in
  Lwt.choose [ conn; send_zip ]

(* **************************************************** *)

let my_zt_ws_main () =
  connect0 ~msg:"zip transfert webssocket" ~react:client_zt_react
    (EzAPI.BASE "http://localhost:8080") Services.zip_tranfer
  >>= function
  | Error e -> error2 e
  | Ok con -> (
      handle_zt con >>= function
      | Error e -> error2 e
      | Ok () ->
          EzDebug.printf "client connection ended properly" ;
          Lwt.return_unit)


let gen_comm_react _send = function
  | Ok s ->
      print_endline @@ default_server_response_to_string s ;
      Lwt.return_ok ()
  | Error exn ->
      (* print_endline "error from client react gen_comm" ; *)
      EzDebug.printf "client react to error: %s" (Printexc.to_string exn) ;
      Lwt.return_ok ()

let gen_comm_handle { conn; action = { send; close } }
    (my_rec : Data_types.general_comm) =
  let rec response () =
    send @@ my_rec >>= function
    | Error _ -> close None
    | Ok () -> Lwt.bind (EzLwtSys.sleep 0.) (fun () -> response ()) in
  Lwt.choose [ conn; response () ]

let react _send = function
  | Ok s ->
      EzDebug.printf "client react: %s" s ;
      Lwt.return_ok ()
  | Error exn ->
      EzDebug.printf "client react to error: %s" (Printexc.to_string exn) ;
      Lwt.return_ok ()

let handle { conn; action = { send; close } } =
  let rec loop i =
    EzDebug.printf "client loop step %d" i ;
    send @@ "client send " ^ string_of_int i >>= function
    | Error _ -> close None
    | Ok () -> Lwt.bind (EzLwtSys.sleep 11.) (fun () -> loop (i + 1)) in
  Lwt.choose [ conn; loop 0 ]

(** Check exchange & switch to zip exchange (not [Data_types.general_comm]) *)
(* let my_ws_main () =
  connect0 ~msg:"custom ws" ~react:gen_comm_react
    (EzAPI.BASE "http://localhost:8080") Services.zip_tranfer
  >>= function
  | Error e -> error2 e
  | Ok con -> (
      gen_comm_handle con Client_utils.Requests_input.g_comm >>= function
      | Error e -> error2 e
      | Ok () ->
          EzDebug.printf "client connection ended properly" ;
          Lwt.return_unit) *)

(* let ws_main () =
  connect0 ~msg:"ws" ~react (EzAPI.BASE "http://localhost:8080")
    Services.service
  >>= function
  | Error e -> error2 e
  | Ok con -> (
      handle con >>= function
      | Error e -> error2 e
      | Ok () ->
          EzDebug.printf "client connection ended properly" ;
          Lwt.return_unit) *)

let () =
  Printexc.record_backtrace true ;
  EzCohttp.init () ;

  EzLwtSys.run my_zt_ws_main
  (* let api = Printf.sprintf "http://localhost:%d" !api_port in
  print_endline ("sending reqs to " ^ api) ;
  let api = BASE api in

  let requests =
    [
      (* base_req2 { basic = "okok" };
         base_req2 { basic = "okok" };
         basic;
         get_jobs { job_client_req = "ocamlpro" };
         get_specific_job {job_client = "ocamlpro"; job_ref_tag_v = 1}; *)
      (* test_session { basic = "okok" }; *)
      (* test_signup Client_utils.Requests_input.fault_email_test_james; *)
      (* test_signup Client_utils.Requests_input.fault_password_test_james; *)
      send_meta_payload Client_utils.Requests_input.metadata_example;
    ] in
  List.iter (fun test -> test api) requests ;
  if !nrequests > 0 then (
    waiting := true ;
    EzLwtSys.run (fun () -> waiter)) ;
  print_endline
    (string_of_bool (check_password_validity "examPle!!//*dc,a22225")) ;
  print_endline
    (string_of_bool (check_email_validity "james.deanddeafgmail.com")) *)
