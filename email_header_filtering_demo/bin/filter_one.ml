(* Copyright (c) 2025, The MITRE Corporation.
*)


(* Retrieve the directories and filenames; 
   Interpret the config file;
   then apply the actions with change *not* permitted.   

   Assumption:

   The variable DEMO_ROOT should be set to the top of this
   project hierarchy, for instance to the value
   "/Users/guttman/scm/LayeredAttestation/src/demo_layered_attestation/"
   below which we have an example source dir, the project
   code dir email_header_filtering_demo/ with bin and lib
   subdirectories, etc.
*) 

let _ = Act.act
    (* directory-determining env var names:  *)
    "CDS_REWRITTEN" "CDS_OUTBOUND" "CDS_LOG"
    (* configuration file name:  argv.[0] concatenated with *) 
    "config.json"
    (* whether to permit rewrites, if this is not the final 
       filtering:  *)
    false 


       
