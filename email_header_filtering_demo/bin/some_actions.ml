(* Copyright (c) 2025, The MITRE Corporation.
*)

open Config_types  
    
let a1 = [Header "Received";
          Header  "Message-ID";
          Header "x-forefront-antispam-report";
          Header "x-ms-exchange-crosstenant-authsource"],
         [ Substitute (Strings ("outlook.com", "mitre.org")) ]

let a2 = [Header "X-MS-Exchange-Organization-AuthSource"],
         [Deletion "outlook.com"] 


let _ = a1, a2


let _ = Set_up_env.set_dirs ()
let _ = Set_up_env.set_consts () 

let _ =
  try
    let filename = Sys.argv.(1) in
    let in_file =
      Filename.concat
        (Sys.getenv "DEMO_ROOT")
        (Filename.concat "example_emails" filename) in
    let out_file =
      Filename.concat
        (Sys.getenv "DEMO_ROOT")
        (Filename.concat "external_arrivals" filename) in
    let log_file =
      Filename.concat
        (Sys.getenv "DEMO_ROOT")
        (Filename.concat "external_arrivals"
           (filename ^ ".log")) in 
      Pipeline.run_and_invoke
        (fun () -> 
           Pipeline.read_write_harness
             (Header_fns.parse_transform_print_email_file
                (Transforms.actions_transform_table [a1;a2])
                true)
             in_file out_file log_file)
  with
  | _ ->
    print_endline ("src: "
                   ^ Filename.concat
                     (Sys.getenv "DEMO_ROOT")
                     (Filename.concat "example_emails" Sys.argv.(1)));
    print_endline ("out: "
                   ^ Filename.concat
                     (Sys.getenv "DEMO_ROOT")
                     (Filename.concat "external_arrivals" Sys.argv.(1)));
    print_endline Sys.argv.(1)



