(* Copyright (c) 2025, The MITRE Corporation.
*)

let _ = Set_up_env.set_dirs ()
let _ = Set_up_env.set_consts ()  

let _ =
  Pipeline.start_server "CDS_INBOUND" "CDS_PIPELINE"
    (Sys.getenv "CDS_IP")
    (int_of_string (Sys.getenv "CDS_PORT"))






