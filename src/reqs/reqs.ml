open Data_types
open Bcrypt
open Utils
open Lwt.Infix
open EzAPI
open EzAPI.TYPES

(* Absolute trash ... Impossible to run any request ... *)

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
  decr nrequests;
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
  Printf.eprintf "Error: request %s returned code %d\n%!" test n;
  exit 2

let basic api =
  begin_request ();
  EzRequest.ANY.get0 ~msg:"simplest req possible" api Services.version
    ~error:(error "even on the dummiest") (function
    | Ok r ->
        Printf.eprintf "Result for Simple req : %s\n%!"
          (version_test_to_string r);
        (* handle_response r *)
        end_request ()
    | Error e ->
        Printf.eprintf "%s\n%!" @@ "very bad error even in basic req";
        (* handle_error e *)
        end_request ())

let base_req2 arg api =
  begin_request ();
  print_endline "sending ==> base_req2";
  EzRequest.ANY.post0 ~msg:"Testing simple req with EzRequest.ANY.post0 // "
    ~error:(error "error classic req; no lwt //")
    ~input:arg api Services.version_test_json_body (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e);
        end_request ()
    | Ok r ->
        Printf.eprintf "Test base_req2 returned ==> %s\n%!"
          (version_test_to_string r);
        end_request ())

let get_jobs arg api =
  begin_request ();
  EzRequest.ANY.post0 ~msg:"getting jobs from user : "
    ~error:(error "Unable to get jobs |get_jobs| request")
    ~input:arg api Services.sr_job_desc_from_user (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e);
        end_request ()
    | Ok r ->
        Printf.eprintf "Jobs : %s\n%!" (job_list_to_string r);
        end_request ())

let get_specific_job arg api =
  begin_request ();
  EzRequest.ANY.post0 ~msg:"Getting Specific job : "
    ~error:(error "Unable to get specific job [get_specific_job] ")
    ~input:arg api Services.sr_job_desc (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e);
        end_request ()
    | Ok r ->
        Printf.eprintf "Job : %s\n%!" (job_list_to_string r);
        end_request ())

let test_session arg api =
  let open EzSession.TYPES in
  begin_request ();
  Session.connect api (function
    | Error _ ->
        Printf.eprintf "Error in Connect \n%!";
        exit 2
    | Ok (Some _u) -> assert false
    | Ok None ->
        Session.login api ~login:"test_user60"
          ~password:
            "$2y$08$i2c6b9CeUpLJQs8ZGqmrie399MFdX2w17KPXqiXO4cIY6fnEO1CUS"
          (* TO DEFINE *) (function
          | Error _ ->
              Printf.eprintf "Error in Login \n%!";
              exit 2
          | Ok u ->
              Printf.eprintf
                "auth_login = %s; auth_token = %s; auth_user_id = %s; \
                 auth_user_info = %s\n\
                 %!"
                u.auth_login u.auth_token u.auth_user_id u.auth_user_info;
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
                      (version_test_to_string r);
                    Session.logout api ~token:u.auth_token (function
                      | Error _ ->
                          Printf.eprintf "Error in logout\n%!";
                          exit 2
                      | Ok bool ->
                          Printf.eprintf "logout OK %b\n%!" bool;
                          end_request ())
                | Error e ->
                    Printf.eprintf "%s\n%!"
                    @@ Printexc.to_string (proofbox_api_error e);
                    end_request ())))

let test_signup arg api =
  begin_request ();
  EzRequest.ANY.post0 ~msg:"Testing signup service : "
    ~error:(error "Unable to Signup User : [test_signup]")
    ~input:arg api Services.sign_up_new_user (function
    | Error e ->
        Printf.eprintf "%s\n%!" @@ Printexc.to_string (proofbox_api_error e);
        end_request ()
    | Ok r ->
        Printf.eprintf "\nDefault response ==> %s\n\n%!"
          (Utils.default_server_response_to_string r);
        end_request ())

let () =
  Printexc.record_backtrace true;
  EzCohttp.init ();
  let api = Printf.sprintf "http://localhost:%d" !api_port in
  print_endline ("sending reqs to " ^ api);
  let api = BASE api in

  let requests =
    [
      (* base_req2 { basic = "okok" };
         base_req2 { basic = "okok" };
         basic;
         get_jobs { job_client_req = "ocamlpro" };
         get_specific_job {job_client = "ocamlpro"; job_ref_tag_v = 1}; *)
      (* test_session { basic = "okok" }; *)
      test_signup Client_utils.Requests_input.fault_email_test_james;
      test_signup Client_utils.Requests_input.fault_password_test_james;
    ]
  in
  List.iter (fun test -> test api) requests;
  if !nrequests > 0 then (
    waiting := true;
    EzLwtSys.run (fun () -> waiter));
  print_endline (string_of_bool (check_password_validity "examPle!!//*dc,a22225"));
  print_endline
    (string_of_bool (check_email_validity "james.deanddeafgmail.com"))
