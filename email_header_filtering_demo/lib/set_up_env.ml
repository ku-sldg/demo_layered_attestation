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



(* Compile-time constant:  whether we're doing testing or a real
   configuration
*)

let is_testing_mode = true
(* false otherwise *) 

let fail_unless_testing () =
  is_testing_mode ||
  failwith
    "fail_unless_testing:  Not testing"

let maybe_put_env v str =
  try
    ignore (fail_unless_testing ()); 
    ignore (Unix.getenv v) 
  with
  | _ ->
    Unix.putenv v str
                        
let maybe_get_env v default =
  try
    if fail_unless_testing ()
    then
      Unix.getenv v
    else
      failwith
        "maybe_get_env:  Inaccessible"
  with
    _ -> default

let is_env_set v =
  try
    ignore (Unix.getenv v); true 
  with
  | _ -> false
    
let set_dirs () =
  List.iter
    (fun (var, dirvar, fn) ->
       maybe_put_env var
         (Filename.concat (Sys.getenv dirvar) fn))
    [ ("CDS_INSTALLED", "DEMO_ROOT", "installed_dir");
      ("CDS_BIN", "CDS_INSTALLED", "bin"); 
      ("CDS_STORE", "CDS_INSTALLED", "demo_pipeline_dirs") ;
      ("CDS_INBOUND", "CDS_STORE", "incoming") ;
      ("CDS_REWRITTEN", "CDS_STORE", "rewritten"); 
      ("CDS_OUTBOUND", "CDS_STORE", "outgoing");
      ("CDS_LOG", "CDS_STORE", "err_log"); 
      ("EXT_ARR", "DEMO_ROOT", "external_arrivals");
      ("CDS_CONFIG", "DEMO_ROOT", "cds_config")
    ]

let set_consts () =
  List.iter
    (fun (var, valstr) ->
       maybe_put_env var valstr)
    [ ("CDS_IP", "127.0.0.1");
      ("CDS_PORT", "1789"); 
      ("CDS_PIPELINE",
       String.concat ":"
         (List.map (fun ex ->
              Filename.concat (Sys.getenv "CDS_BIN") ex)
             ["rewrite_one";
              "filter_one"; "export_one"]));
      ("EXT_IP", "127.0.0.1"); 
      ("EXT_PORT", "1848")
    ]
