(* let () =
  Printexc.record_backtrace true;
  let test = Otoml.Parser.from_file (path_to_toml ^ "/job.toml") in
  test
  |> Otoml.Printer.to_channel ~indent_width:4 ~indent_subtables:true
       ~collapse_tables:false stdout;
  print_endline "\n ==> getting access to nested +/- values\n";

  print_endline "=> email : ";
  Otoml.find test Otoml.get_value [ "owner"; "email" ]
  |> Otoml.Printer.to_channel stdout;

  print_endline "\n=> job_description table : ";
  get_owner_bio test (* |> Otoml.Printer.to_string *) |> print_endline;

  print_endline "=> list files ==> :";
  launch_process path_to_toml;

  let jdptof = get_str test [ "job_description"; "path_to_client_repo" ] in
  (* stringlist_printer
  @@ get_all_files_w_ext_smts
       "/home/elias/OCP/PROOFBOX_TestJobs/job_example1/ALIA/piVC"; *)
  stringlist_printer @@ dir_contents jdptof;
  
  print_endline @@ Printf.sprintf "Unix.getcwd : %s" (testunix ());
  print_endline "==> Main TOML : =>";
  print_endline get_main_toml;
  try let res_table = retrieve_toml_values in
  Hashtbl.iter
      (fun x y -> print_endline @@ Printf.sprintf "%s %s" x y)
      res_table
  with
    | Toml_error e -> print_endline @@ err_toml_print e
(* print_endline "bad toml format"
   | Toml_not_found ->  @@ toml not found"
   | Unknown _ unknown"sprintf "%d" (List.length @@ get_all_files_w_ext baseTOML_dir ".toml") *) *)