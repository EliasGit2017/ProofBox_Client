open Otoml
open Read_write_toml.Utils
open Data_types
open Utils

let baseTOML_dir =
  "/home/elias/OCP/PROOFBOX_TestJobs/job_example1/custom_batch/"

let zip_output = "/home/elias/OCP/ez_pb_client/example.zip"

(*****************************************************************************)

(* Build metadata for zip and reqs builder *)

let metadata_reader archive_filename =
  let archive = Zip.open_in archive_filename in
  List.iter
    (fun x -> print_endline @@ zip_entry_to_string x)
    (Zip.entries archive)

(*****************************************************************************)

(** Retrieves the list of files [string]s reprensenting : absolute path +
    filename *)
let get_files parsed_toml = dir_contents @@ get_jd_path_tof parsed_toml

let () =
  Printexc.record_backtrace true ;
  print_endline @@ "Main Toml found at " ^ get_main_toml baseTOML_dir ;
  stringlist_printer @@ (get_main_toml baseTOML_dir :: dir_contents baseTOML_dir) ;
  (* metadata_reader zip_output *)
  make_zipbundle ~keep_dir_struct:false baseTOML_dir
    "/home/elias/OCP/ez_pb_client/light.zip"
