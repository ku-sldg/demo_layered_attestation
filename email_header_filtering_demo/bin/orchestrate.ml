(* Copyright (c) 2025, The MITRE Corporation.
*)

let _ = Set_up_env.set_dirs ()
let _ = Set_up_env.set_consts () 


let _ =
  let intake_path =
    Filename.concat (Sys.getenv "CDS_BIN")
      "intake_server" in
  let receipt_path =
    Filename.concat (Sys.getenv "CDS_BIN")
      "receive_externally" in
  (*  print_endline intake_path; 
      print_endline (Sys.getenv "CDS_PIPELINE"); *) 
  let in_pr = Unix.create_process
      intake_path
      [| intake_path |]
      Unix.stdin Unix.stdout Unix.stderr in
  let rc_pr = Unix.create_process
      receipt_path
      [| receipt_path |]
      Unix.stdin Unix.stdout Unix.stderr in
  print_endline
    "started intake_server and receive_externally";
  print_endline
    "to kill them, type: ";
  print_endline("kill -15 " ^
                (string_of_int in_pr) ^ " " ^ 
                (string_of_int rc_pr))
