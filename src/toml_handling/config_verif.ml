open Otoml
open Read_write_toml.Utils
open Data_types
open Utils

let baseTOML_dir = "/home/elias/OCP/PROOFBOX_TestJobs/job_example1/"

let get_main_toml =
  let res = get_all_files_w_ext baseTOML_dir ".toml" in
  if List.length res = 0 then (
    print_endline "Start thinking about errors and exceptions" ;
    (* Throw exception here *)
    "No TOML found")
  else baseTOML_dir ^ List.hd res
(* get absolute path through Sys ? *)

let retrieve_toml_values =
  let parsed_toml = Otoml.Parser.from_file get_main_toml in
  let htab_toml_values = Hashtbl.create 10 in
  try
    Hashtbl.add htab_toml_values "title" (get_title parsed_toml) ;
    Hashtbl.add htab_toml_values "owner_username"
      (get_owner_username parsed_toml) ;
    Hashtbl.add htab_toml_values "owner_email" (get_owner_email parsed_toml) ;
    Hashtbl.add htab_toml_values "owner_bio" (get_owner_bio parsed_toml) ;
    Hashtbl.add htab_toml_values "owner_password"
      (get_owner_password parsed_toml) ;
    Hashtbl.add htab_toml_values "jd_job_id"
      (string_of_int @@ get_jd_job_id parsed_toml) ;
    Hashtbl.add htab_toml_values "jd_solver" (get_jd_solver parsed_toml) ;
    Hashtbl.add htab_toml_values "jd_solver_version"
      (get_jd_solver_version parsed_toml) ;
    Hashtbl.add htab_toml_values "jd_synopsis" (get_jd_job_synopsis parsed_toml) ;
    Hashtbl.add htab_toml_values "jd_path_to_client_repo"
      (get_jd_path_tof parsed_toml) ;
    htab_toml_values
  with Type_error _ | Key_error _ ->
    print_endline @@ Printf.sprintf "Badly formatted TOML at : %s" get_main_toml ;
    raise (Toml_error Bad_toml_format)

(** Retrieves the list of files [string]s reprensenting : absolute path +
    filename *)
let get_files parsed_toml = dir_contents @@ get_jd_path_tof parsed_toml

let () =
  Printexc.record_backtrace true ;
  print_endline @@ "Main Toml found at " ^ get_main_toml ;
  make_zipbundle ~keep_dir_struct:false baseTOML_dir
    "/home/elias/OCP/ez_pb_client/example.zip"
