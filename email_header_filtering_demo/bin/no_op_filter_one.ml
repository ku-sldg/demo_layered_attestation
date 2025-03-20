(* Copyright (c) 2025, The MITRE Corporation.
*)

let _ = Set_up_env.set_dirs ()
let _ = Set_up_env.set_consts () 

let _ =
  try
    let filename = Sys.argv.(1) in 
    match
      List.map (fun var ->
          Filename.concat
            (Sys.getenv var) filename)
        ["CDS_REWRITTEN"; "CDS_OUTBOUND"; "CDS_LOG"] with
    | [in_file; out_file; log_file] -> 
      Pipeline.run_and_invoke
        (fun () -> 
           Pipeline.read_write_harness
             (fun infile outfile _->
                let (_,_) =
                  Read_config.interpret_config_file 
                    (Filename.concat 
                       (Sys.getenv "CDS_CONFIG")
                       "filter_one_config.json") in 
                Pipeline.read_input_ch_to_file 
                  (open_in infile) 
                  outfile;
                true)
             in_file out_file log_file)
    | _ -> ()  
  with
  | _ ->
    print_endline ("CDS_REWRITTEN: "
                   ^ (Sys.getenv "CDS_REWRITTEN"));
    print_endline ("CDS_OUTBOUND: "
                   ^ (Sys.getenv "CDS_OUTBOUND"));
    print_endline Sys.argv.(1)


    
       
