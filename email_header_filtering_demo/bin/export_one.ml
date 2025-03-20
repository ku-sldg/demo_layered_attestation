(* Copyright (c) 2025, The MITRE Corporation.
*)

let _ = Set_up_env.set_dirs ()
let _ = Set_up_env.set_consts () 

let _ =
  try
    let filename = Sys.argv.(1) in 
    let in_file =
      Filename.concat
        (Sys.getenv "CDS_OUTBOUND") filename in       
    Pipeline.run_and_invoke
      (fun () ->
         Pipeline.write_file_to_socket_addr
           in_file
           (Unix.ADDR_INET
              (Unix.inet_addr_of_string (Sys.getenv "EXT_IP"),
               int_of_string (Sys.getenv "EXT_PORT"))));
    Sys.remove in_file
  with
  | _ ->
    print_endline ("Arg: " ^ Sys.argv.(1) );
    print_endline ("CDS_OUTBOUND: " ^ (Sys.getenv "CDS_OUTBOUND"));
    print_endline ("EXT_IP: " ^ (Sys.getenv "EXT_IP"));
    print_endline ("EXT_PORT: " ^ (Sys.getenv "EXT_PORT"))
         
