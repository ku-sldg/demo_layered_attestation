(* Copyright (c) 2025, The MITRE Corporation.
*)

let _ = Set_up_env.set_dirs ()
let _ = Set_up_env.set_consts ()  

let _ =
  Pipeline.start_recipient_server
    (Sys.getenv "EXT_ARR") 
    (Sys.getenv "EXT_IP")
    (int_of_string (Sys.getenv "EXT_PORT"))

    




