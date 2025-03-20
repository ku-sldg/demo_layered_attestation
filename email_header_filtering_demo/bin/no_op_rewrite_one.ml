(* Copyright (c) 2025, The MITRE Corporation.
*)

(* Set up environment variables and launch the initial processes.

   Assumption:

   The variable DEMO_ROOT should be set to the top of this
   project hierarchy, for instance to the value
   "/Users/guttman/scm/LayeredAttestation/src/demo_layered_attestation/"
   below which we have an example source dir, the project
   code dir email_header_filtering_demo/ with bin and lib
   subdirectories, etc.
*) 

let _ = Set_up_env.set_dirs ()
let _ = Set_up_env.set_consts () 

let _ =
  try
    let filename = Sys.argv.(1) in 
    match List.map (fun var ->
        Filename.concat
          (Sys.getenv var) filename)
        ["CDS_INBOUND"; "CDS_REWRITTEN"; "CDS_LOG"] with
    | [in_file; out_file; log_file] -> 
      Pipeline.run_and_invoke
        (fun () -> 
           Pipeline.read_write_harness
             (fun infile outfile _ ->
                Pipeline.read_input_ch_to_file 
                  (open_in infile)
                  outfile;
                true)
             in_file out_file log_file)
    | _ -> ()  
  with
  | _ ->
    print_endline ("CDS_INBOUND: "
                   ^ (Sys.getenv "CDS_INBOUND"));
    print_endline ("CDS_REWRITTEN: "
                   ^ (Sys.getenv "CDS_REWRITTEN"));
    print_endline Sys.argv.(1)


