open Otoml
open Read_write_toml.Utils
open Data_types
open Utils

let baseTOML_dir = "/home/elias/OCP/PROOFBOX_TestJobs/job_example1/"
let zip_output = "/home/elias/OCP/ez_pb_client/example.zip"

let get_main_toml =
  let res = get_all_files_w_ext baseTOML_dir ".toml" in
  if List.length res = 0 then (
    print_endline "Start thinking about errors and exceptions" ;
    (* Throw exception here *)
    "No TOML found")
  else baseTOML_dir ^ List.hd res
(* get absolute path through Sys ? *)

(*****************************************************************************)

(* Build metadata for zip and reqs builder *)

let zip_entry_to_string (z_entry : Zip.entry) =
  Printf.sprintf
    "filename = %s; extra = %s; comment = %s; methd = %s; mtime = %f; crc = \
     %d; uncompressed_size = %d; compressed_size = %d; is_directory = %b; \
     file_offset = %d"
    z_entry.filename z_entry.extra z_entry.comment
    (match z_entry.methd with Stored -> "Stored" | Deflated -> "Deflated")
    z_entry.mtime (Int32.to_int z_entry.crc) z_entry.uncompressed_size
    z_entry.compressed_size z_entry.is_directory
    (Int64.to_int z_entry.file_offset)

let metadata_extractor =
  let archive = Zip.open_in zip_output in
  List.iter (fun x -> print_endline @@ zip_entry_to_string x) (Zip.entries archive)

(*****************************************************************************)

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
  print_endline @@ "Main Toml found at " ^ get_main_toml;
  metadata_extractor
(* make_zipbundle ~keep_dir_struct:false baseTOML_dir
   "/home/elias/OCP/ez_pb_client/example.zip" *)
