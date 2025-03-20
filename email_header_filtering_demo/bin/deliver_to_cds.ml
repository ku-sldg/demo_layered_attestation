(* Copyright (c) 2025, The MITRE Corporation.
*)

let _ = Set_up_env.set_dirs ()
let _ = Set_up_env.set_consts () 

(*
let _ = print_endline "started"
let _ = flush stdout
*)

let src_dir =
  if Array.length Sys.argv <= 1
  then "example_emails"
  else Sys.argv.(1)

let _ =
  Pipeline.transmit_msgs_in_dir
    (Filename.concat
       (Sys.getenv "DEMO_ROOT")
       src_dir) 
    (Unix.ADDR_INET
       (Unix.inet_addr_of_string (Sys.getenv "CDS_IP"),
        int_of_string (Sys.getenv "CDS_PORT")))



