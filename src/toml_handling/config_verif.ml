open Otoml
open Read_write_toml.Utils
open Data_types
open Utils

let baseTOML_dir = "/home/elias/OCP/PROOFBOX_TestJobs/job_example1/"

let get_main_toml =
  let res = get_all_files_w_ext baseTOML_dir ".toml" in
  if List.length res = 0 then (
    print_endline "Start thinking about errors and exceptions";
    (* Throw exception here *)
    "No TOML found")
  else baseTOML_dir ^ List.hd res
(* get absolute path through Sys ? *)

let retrieve_toml_values =
  let parsed_toml = Otoml.Parser.from_file get_main_toml in
  let htab_toml_values = Hashtbl.create 10 in
  try
    Hashtbl.add htab_toml_values "title" (get_title parsed_toml);
    Hashtbl.add htab_toml_values "owner_username"
      (get_owner_username parsed_toml);
    Hashtbl.add htab_toml_values "owner_email" (get_owner_email parsed_toml);
    Hashtbl.add htab_toml_values "owner_bio" (get_owner_bio parsed_toml);
    Hashtbl.add htab_toml_values "owner_password"
      (get_owner_password parsed_toml);
    Hashtbl.add htab_toml_values "jd_job_id"
      (string_of_int @@ get_jd_job_id parsed_toml);
    Hashtbl.add htab_toml_values "jd_solver" (get_jd_solver parsed_toml);
    Hashtbl.add htab_toml_values "jd_solver_version"
      (get_jd_solver_version parsed_toml);
    Hashtbl.add htab_toml_values "jd_synopsis" (get_jd_job_synopsis parsed_toml);
    Hashtbl.add htab_toml_values "jd_path_to_client_repo"
      (get_jd_path_tof parsed_toml);
    htab_toml_values
  with Type_error _ | Key_error _ ->
    print_endline @@ Printf.sprintf "Badly formatted TOML at : %s" get_main_toml;
    raise (Toml_error Bad_toml_format)

(** Retrieves the list of files [string]s reprensenting : absolute path + filename *)
let get_files parsed_toml = dir_contents @@ get_jd_path_tof parsed_toml

let zip_bundle path_fn target =
  let bundle_zip = Gzip.open_out_chan ~level:7 path_fn in
  Gzip.output bundle_zip target 0 (Bytes.length target);
  Gzip.close_out bundle_zip

let get_bytes fn =
  let inc = open_in_bin fn in
  let rec go sofar =
    match input_byte inc with
    | b -> go (b :: sofar)
    | exception End_of_file -> List.rev sofar
  in
  let res = go [] in
  close_in inc;
  res

(** [write_gzipped_file file_name l s] writes to the file [file_name] 
    the gzipped contents of string [s]. *)
let write_gzipped_file (file_name : string) (s : string) : unit =
  try
    let n = String.length s in
    if n > 0 then (
      let f = Gzip.open_out ~level:7 file_name in
      Gzip.output_substring f s 0 n;
      (* Gzip.close_out f *))
    else
      (* Zero length string: we write a zero length file. TO DO -> change to not creating file *)
      let f = open_out file_name in
      close_out f
  with Gzip.Error s -> raise (Gzip.Error (s ^ " writing file: " ^ file_name))

let make_zip_bundle dir_name =
  let target_files = dir_contents dir_name in
  List.iter
    (fun e ->
      let tmp = open_in e in
      write_gzipped_file e (really_input_string tmp (in_channel_length tmp)))
    target_files

let () =
  Printexc.record_backtrace true;
  make_zip_bundle baseTOML_dir
  (* let all_files = dir_contents baseTOML_dir in
  let to_comp = open_in (List.hd all_files) in
  let e = really_input_string to_comp (in_channel_length to_comp) in
  write_gzipped_file (List.hd all_files ^ ".zip") e *)

(* let test = Otoml.Parser.from_file (path_to_toml ^ "/job.toml") in
   stringlist_printer @@ dir_contents @@ (test |> get_jd_path_tof) *)

(* let write_file path string =
   let out_gzip = Gzip.open_out path in
   Gzip.output out_gzip string 0 (String.length string);
   Gzip.close_out out_gzip *)
