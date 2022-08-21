open Otoml
open Str

(* Otoml : Utils to get / set values and acces toml files simply &
   Wrappers over some functions : https://github.com/dmbaturin/otoml*)

let title_toml = [ "title" ]
let owner_username = [ "owner"; "username" ]
let owner_email = [ "owner"; "email" ]
let owner_bio = [ "owner"; "bio" ]
let owner_password = [ "owner"; "password" ]
let job_description_id = [ "job_description"; "job_id" ]
let job_description_solver = [ "job_description"; "solver" ]
let job_description_solver_version = [ "job_description"; "solver_version" ]
let job_description_synopsis = [ "job_description"; "synopsis" ]
let job_description_path_tof = [ "job_description"; "path_to_client_repo" ]

(** Bad list to str *)
let stringl_to_str s_l = List.fold_left ( ^ ) "" s_l

(** change Otoml.TomlBoolean to bool (decaps/encaps) *)
let get_value_wrap parsed_toml path_toval =
  try Otoml.find parsed_toml Otoml.get_value path_toval with
  | Type_error e ->
      print_endline
      @@ Printf.sprintf "Otoml.Type_error from get_value_wrap : %s" e;
      Otoml.TomlBoolean false
  | Key_error e ->
      print_endline
      @@ Printf.sprintf "Otoml.Type_error from get_value_wrap : %s" e;
      Otoml.TomlBoolean false

let get_str parsed_toml path_toval =
  Otoml.find parsed_toml (Otoml.get_string ~strict:true) path_toval

let get_int parsed_toml path_toval =
  Otoml.find parsed_toml (Otoml.get_integer ~strict:true) path_toval

let get_title parsed_toml = get_str parsed_toml title_toml
let get_owner_username parsed_toml = get_str parsed_toml owner_username
let get_owner_email parsed_toml = get_str parsed_toml owner_email
let get_owner_bio parsed_toml = get_str parsed_toml owner_bio
let get_owner_password parsed_toml = get_str parsed_toml owner_password
let get_jd_job_id parsed_toml = get_int parsed_toml job_description_id
let get_jd_solver parsed_toml = get_str parsed_toml job_description_solver

let get_jd_solver_version parsed_toml =
  get_str parsed_toml job_description_solver_version

let get_jd_job_synopsis parsed_toml =
  get_str parsed_toml job_description_synopsis

let get_jd_path_tof parsed_toml = get_str parsed_toml job_description_path_tof

(* Unix : tools *)

let path_to_toml = "/home/elias/OCP/ez_proofbox/src/backend_arch"
let testunix = Unix.getcwd
let accepted_formats = [ ".smt2"; ".ae" ]

(** Returns a string describing Unix error status code *)
let stat_code status =
  match status with
  | Unix.WEXITED e -> Printf.sprintf "WEXITED : code = %d" e
  | Unix.WSIGNALED s -> Printf.sprintf "WSIGNALED : code = %d" s
  | Unix.WSTOPPED st -> Printf.sprintf "WSTOPPED : code = %d" st

(** Print channel with [print_endline]  *)
let print_chan channel =
  let rec loop () =
    let () = print_endline (input_line channel) in
    loop ()
  in
  try loop () with End_of_file -> close_in channel

(** see jane street existing functions :
    In_channel.read_all "./input.txt"
    In_channel.read_lines "./input.txt"
    In_channel.fold_lines *)
let chan_to_stringlist channel =
  let rec loop acc =
    try loop (input_line channel :: acc) with End_of_file -> List.rev acc
  in
  loop []

(** Print string list with [print_endline] for each element *)
let rec stringlist_printer = function
  | [] -> ()
  | e :: l ->
      print_endline e;
      stringlist_printer l

(** Convert string list to string with [sep] as separator *)
let rec sringlist_tostring sep = function
  | [] -> ""
  | "" :: l -> sringlist_tostring sep l
  | e :: l -> e ^ sep ^ sringlist_tostring sep l

(* Alternatives *)
(* let join l = List.filter (fun s -> s <> "") l |> String.concat "" *)
(* let join2 sep l = String.concat sep l *)

let get_all_files_w_ext_smts wd =
  Sys.readdir wd |> Array.to_list
  |> List.filter (fun x ->
         Filename.extension x = ".smt2" || Filename.extension x = ".ae")

let get_all_files_w_ext wd ext =
  Sys.readdir wd |> Array.to_list
  |> List.filter (fun x -> Filename.extension x = ext)

(** [dir_contents] returns the paths of all regular files ([.smt2] && [.ae]) that are
  contained in [dir]. Each file is a path starting with [dir].*)
let dir_contents dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] ->
        result
        |> List.filter (fun x ->
               Str.string_match (Str.regexp {|\(.*\.smt2\)\|(.*\.ae)|}) x 0)
    (* not optimized *)
  in
  loop [] [ dir ]

(** Remove dir_name from filename *)
let remove_fn_dir (file_name : string) : string =
  Filename.basename file_name

(** List version of [remove_fn_dir] *)
let l_remove_fn_dir (file_list : string list) : string list =
  List.map (fun e -> remove_fn_dir e) file_list

(** Exploration && tests *)
let launch_process base_dir =
  let ((ocaml_stdout, ocaml_stdin, ocaml_stderr) as p) =
    Unix.open_process_args_full "/usr/bin/ls"
      [| "/usr/bin/ls"; path_to_toml |]
      (Unix.environment ())
  in
  let l_res = ref [] in
  (* print_endline @@ Printf.sprintf "%d" @@ List.length @@ chan_to_stringlist ocaml_stdout;
     print_endline @@ Printf.sprintf "%d" @@ List.length @@ chan_to_stringlist ocaml_stderr; *)
  stringlist_printer @@ chan_to_stringlist ocaml_stdout;
  stringlist_printer @@ chan_to_stringlist ocaml_stderr;

  stringlist_printer !l_res;

  let stat = Unix.close_process_full p in
  print_endline @@ stat_code stat


(* camlzip utils & tools *)

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
    the gzipped contents of string [s]. (Deprecated) *)
let write_gzipped_file (file_name : string) (s : string) : unit =
  try
    let n = String.length s in
    if n > 0 then (
      let f = Gzip.open_out ~level:7 file_name in
      Gzip.output_substring f s 0 n;
      Gzip.close_out f)
    else
      (* Zero length string: we write a zero length file. TO DO -> change to not creating file *)
      let f = open_out file_name in
      close_out f
  with Gzip.Error s -> raise (Gzip.Error (s ^ " writing file: " ^ file_name))

(** [make_zipbundle ~keep_dir_struct:false dir_name archive_name] creates 
    a zip containing the [.smt2] & [.ae] files present in the directory [dir_name]
    (absolute path). The directory structure is not preserved when [keep_dir_struct]
    is set to false as all the files are bundled together in the same main zipped directory. *)
let make_zipbundle (dir_name : string) (archive_name : string)
    ?(keep_dir_struct = true) : unit =
  try
    let target_files = dir_contents dir_name in
    let main_archive =
      Zip.open_out ~comment:"Main archive containing target files" archive_name
    in
    List.iter
      (fun e ->
        if keep_dir_struct then
          Zip.copy_file_to_entry ~extra:"target_file for smt solver"
            ~comment:"to be solved" ~level:7 e main_archive e
        else
          Zip.copy_file_to_entry ~extra:"target_file for smt solver"
            ~comment:"to be solved" ~level:7 e main_archive (remove_fn_dir e))
      target_files;
    Zip.close_out main_archive
  with Zip.Error _ ->
    raise
    @@ Zip.Error
         ( archive_name,
           "unspecified filename",
           " problem when writting files to archive" )