open Data_types
open Str
open EzAPI

(* Conversion & data printing : ( *_to_string, *_of_string, etc) *)

(** [to_result str ~convf] encapsulates application of [convf] on [str] within
    [result] type *)
let to_result :
    type conv. string -> convf:(string -> conv) -> (conv, string) result =
 fun str ~convf ->
  try Ok (convf str)
  with Failure str -> Error ("Not recognized data_type : " ^ str)

(* ********* Might not be so usefull *********** *)

(** Create [Data_types.version] from string where record field are separated by
    '+' (to generalize) *)
let version_test_of_string str =
  match String.split_on_char '+' str with
  | [ v_db; v_db_version ] ->
      let v_db_version = int_of_string v_db_version in
      { v_db; v_db_version }
  | _ -> failwith ("Not valid entry info : " ^ str)

(** Create string describing [Data_types.version] with '+' as separator (to
    genralize) *)
let version_test_to_string { v_db; v_db_version } =
  Printf.sprintf "{v_db = %s ; v_db_version = %d}" v_db v_db_version

(** Create string describing [Data_types.user_description] . Here for
    debugging/verbose purposes only. *)
let users_to_string { username; email; password; user_desc; first_login_date } =
  Printf.sprintf
    "username = %s\n\
     email = %s\n\
     password = %s\n\
     user_description = %s\n\
     first_login_date = %s\n"
    username email password user_desc first_login_date

(** [Data_types.jobs] to string : risk to overflow the stack *)
let job_list_to_string job_l =
  List.fold_left
    (fun res { job_client; job_ref_tag; order_ts; path_to_f; priority; status } ->
      res
      ^ Printf.sprintf
          "{ job_client = %s; job_ref_tag = %d; order_ts = %s; path_to_f =%s ; \
           priority = %d; status = %s }\n"
          job_client job_ref_tag order_ts path_to_f priority status)
    "" job_l

let default_server_response_from_string _comm_des _client_infos _infos
    error_desc =
  {
    comm_desc = _comm_des;
    client_infos = _client_infos;
    infos = _infos;
    error_desc;
  }

let default_server_response_to_string elem =
  Printf.sprintf
    "comm_desc = %s; client_infos = %s; infos = %s; error_desc = %s"
    elem.comm_desc elem.client_infos elem.infos elem.error_desc

let meta_payload_to_string (meta : Data_types.meta_payload) =
  Printf.sprintf
    "archive_name = %s; client_id = %s; comment = %s; priority = %d; checksum_type = %s; \
     checksum = %s; info = %s; error = %s; code = %d"
    meta.archive_name meta.client_id meta.comment meta.priority meta.checksum_type
    meta.checksum meta.info meta.error meta.code

let meta_payload_from_string archive_name client_id comment priority
    checksum_type checksum info error code =
  {
    archive_name;
    client_id;
    comment;
    priority;
    checksum_type;
    checksum;
    info;
    error;
    code;
  }

(* ************************************************************************* *)

(** Regex check on email : pattern identical to domain attempt in
    [db/versions.ml] *)
let check_email_validity email =
  let right_email = Str.regexp "\\([^<>(),; \t]+@[^<>(),; \t]+\\)$" in
  Str.string_match right_email email 0

(** Regex check on password rules : At least one digit [0-9] At least one
    lowercase character [a-z] At least one uppercase character [A-Z] ** At least
    one special character [\[*.!@#$%^&(){}\[\]:;<>,.?/~_+-=|\]] At least 8
    characters in length, but no more than 32.*)
let check_password_validity password =
  let right_password =
    Str.regexp {|^\(.{0,7}\|[^0-9]*\|[^A-Z]*\|[^a-z]*\|[a-zA-Z0-9]*\)$|} in
  not @@ Str.string_match right_password password 0

(* ************************************************************************* *)

(* Error printing *)

let err_toml_print e =
  match e with
  | Bad_toml_format -> "Toml_error.bad_toml_format"
  | Toml_not_found -> "Toml_error.toml_not_found"
  | Unknown -> "Toml.error.unknown"

(*****************************************************************************)

(* websocket handling *)
(* Think about ws extra protocol to retrieve filename *)

let mime_getter = [ Option.get @@ Mime.parse "application/zip" ]

