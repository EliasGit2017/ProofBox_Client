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



let error test n =
  Printf.eprintf "Error: request %s returned code %d\n%!" test n;
  exit 2

let basic api =
  begin_request ();
  EzRequest.ANY.get0 ~msg:"simplest req possible" api Services.version
    ~error:(error "even on the dummiest") (function
    | Ok r ->
        Printf.eprintf "Result for Simple req : %s\n%!"
          (Utils.version_test_to_string r);
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
          (Utils.version_test_to_string r);
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
        Printf.eprintf "Jobs : %s\n%!" (Utils.job_list_to_string r);
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
        Printf.eprintf "Job : %s\n%!" (Utils.job_list_to_string r);
        end_request ())

let () =
  Printexc.record_backtrace true;
  EzCohttp.init ();
  let api = Printf.sprintf "http://localhost:%d" !api_port in
  print_endline ("sending req 0 to " ^ api);
  let api = BASE api in

  let requests =
    [
      base_req2 { basic = "okok" };
      base_req2 { basic = "okok" };
      basic;
      get_jobs { job_client_req = "ocamlpro" };
      get_specific_job {job_client = "ocamlpro"; job_ref_tag_v = 1};
    ]
  in
  List.iter (fun test -> test api) requests;
  if !nrequests > 0 then (
    waiting := true;
    EzLwtSys.run (fun () -> waiter));
  (* print_endline (Printf.sprintf "\n %s" (Utils.job_list_to_string jobs)) *)

