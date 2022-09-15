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

(** Source for test zip *)
let ztest = "/home/elias/OCP/ez_pb_client/light.zip"

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

let get_cache arg api =
  begin_request () ;
  EzRequest.ANY.post0 ~msg:"getting jobs from user : "
    ~error:(error "Unable to get jobs |get_jobs| request")
    ~input:arg api Services.consult_cache (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e) ;
        end_request ()
    | Ok r ->
        Printf.eprintf "Cache :\n%s\n%!" (cache_list_to_string r.job_return) ;
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

let signup arg api =
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

(* Websocket client test *)

let g_base = ref "" (* used to store checksum obtained from ws *)

let client_zt_react _send = function
  | Ok s ->
      EzDebug.printf "client got : { %s } from server" s ;
      g_base := s ;
      Lwt.return_ok ()
  | Error e ->
      EzDebug.printf "client got error from server : %s"
        (Printexc.to_string (Proofbox_api_error e)) ;
      Lwt.return_ok ()

let handle_zt { conn; action = { send; close } } =
  let rec send_zip () =
    if compare (String.split_on_char ' ' !g_base |> List.hd) "echo" = 0 then (
      EzDebug.printf "Stop connection" ;
      g_base := "" ;
      Lwt.return_ok ())
    else (
      EzDebug.printf "sending zip in client handler" ;
      send @@ zip_to_str ztest >>= function
      | Error _ -> close None
      | Ok () -> Lwt.bind (EzLwtSys.sleep 5.) (fun () -> send_zip ())) in
  Lwt.choose [ conn; send_zip () ]

(* **************************************************** *)

let my_zt_ws_main () =
  begin_request () ;
  connect0 ~msg:"zip transfert webssocket" ~react:client_zt_react
    (EzAPI.BASE "http://localhost:8080") Services.zip_tranfer
  >>= function
  | Error e -> error2 e
  | Ok con -> (
      handle_zt con >>= function
      | Error e -> error2 e
      | Ok () ->
          EzDebug.printf "client connection ended properly" ;
          end_request () ;
          Lwt.return_unit)

let glob_zip_test arg api =
  begin_request () ;
  EzRequest.ANY.post0 ~msg:"Testing zip send blob : "
    ~error:(error "blob zip failed") ~input:arg api Services.post_zip_send
    (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e) ;
        end_request ()
    | Ok r ->
        EzDebug.printf "go check file" ;
        Printf.eprintf "%s" (Utils.gen_comm2_to_string r) ;
        end_request ())

let main_send_zip arg api =
  begin_request () ;
  EzRequest.ANY.post0 ~msg:"Main server send zip v1 : "
    ~error:(error "Blob ZIP SEND MAIN FAILED")
    ~input:arg api Services.send_job_main_service (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e) ;
        end_request ()
    | Ok r ->
        Printf.eprintf "Return Json for main send : \n %s"
          (job_payload_to_string r) ;
        end_request ())

let retrieve arg api =
  begin_request () ;
  EzRequest.ANY.post0 ~msg:"Main server retrieve zip v1 : "
    ~error:(error "Blob ZIP RETRIEVE MAIN FAILED")
    ~input:arg api Services.retrieve_job_result (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e) ;
        end_request ()
    | Ok r ->
        Printf.eprintf "Return Json for main send : \n %s"
          (job_payload_to_string r) ;
        Utils.write_to_dest arg.checksum
          r.infos_pb ;
        end_request ())

(** Request to send meta_payload : returns all the jobs associated to the client
    who initiated the exchange *)
let send_meta_payload arg api =
  begin_request () ;
  EzRequest.ANY.post0
    ~msg:
      "writting job in db and retrieving all jobs from user => initiating ws \
       to transfer zip"
    ~error:(error "Unable to send meta_payload & send job to server")
    ~input:arg api Services.send_job_metadata (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e) ;
        end_request ()
    | Ok r ->
        Printf.eprintf "Jobs : %s\n%!" (job_list_to_string r) ;
        (* run websocket once job is inserted in db all all given jobs are returned *)
        (* let _ = my_zt_ws_main () in *)
        main_send_zip
          {
            R_i.Requests_input.job_payload_example with
            infos_pb = get_bytes ztest;
          }
          api ;
        end_request ())

(* **************************************************** *)

let () =
  Printexc.record_backtrace true ;
  EzCohttp.init () ;
  (* EzLwtSys.run my_zt_ws_main  *)
  let api = Printf.sprintf "http://localhost:%d" !api_port in
  let api = BASE api in
  let requests = ref [] in
  let open Stdlib in
  let open Read_write_toml in
  Arg.parse
    [
      ( "--send-job",
        Arg.String
          (fun s ->
            let e = Sys.getcwd () in
            let zip_path = e ^ "/jobs.zip" in
            Utils.make_zipbundle ~keep_dir_struct:false s zip_path ;
            requests :=
              main_send_zip
                {
                  R_i.Requests_input.job_payload_example with
                  infos_pb = get_bytes zip_path;
                }
              :: !requests),
        "Send job to server" );
      ( "--retrieve-job",
        Arg.String
          (fun s ->
            let l_s = String.split_on_char ':' s in
            requests :=
              retrieve
                {
                  R_i.Requests_input.job_payload_example with
                  desc = List.hd l_s;
                  checksum = List.nth l_s 1;
                }
              :: !requests),
        "Retrieve job from server once an email is received" );
      ( "--signup",
        Arg.String
          (fun s ->
            let l_s = String.split_on_char '/' s in
            let user =
              {
                username = List.hd l_s;
                email = List.nth l_s 1;
                password = List.nth l_s 2;
                user_desc = List.nth l_s 3;
                first_login_date =
                  CalendarLib.Printer.Time.to_string @@ CalendarLib.Time.now ();
              } in
            requests := signup user :: !requests),
        "Signup new user : username/email/password/user_desc don't forget to \
         \\ spaces and slash chars" );
      ( "--consult-jobs",
        Arg.String
          (fun s ->
            let user = { job_client_req = s } in
            requests := get_jobs user :: !requests),
        "Consult all jobs on the server from : username (jobs with status done \
         as well as jobs with status scheduled)" );
      ( "--consult-specific-job",
        Arg.String
          (fun s ->
            let l_s = String.split_on_char '/' s in
            let job_tag =
              {
                job_client = List.hd l_s;
                job_ref_tag_v = int_of_string (List.nth l_s 1);
              } in
            requests := get_specific_job job_tag :: !requests),
        "Consult specific job on the server from : username with id" );
      ( "--consult-cache",
        Arg.String
          (fun s ->
            let l_s = String.split_on_char '/' s in
            let job_tag =
              {
                job_archive_name = "None";
                job_client_id = List.hd l_s;
                desc = List.nth l_s 1;
                job_return = [];
                code = 0;
              } in
            requests := get_cache job_tag :: !requests),
        "Consult cache stored on the server for : username/desc " );
    ]
    (fun s -> Printf.eprintf "Error: unexpected argument %S\nAborting.\n%!" s)
    "proofbox-client [--cmd] < Options : ... change to cmdliner when time \
     available >" ;

  List.iter (fun test -> test api) !requests ;
  if !nrequests > 0 then (
    waiting := true ;
    EzLwtSys.run (fun () -> waiter))
