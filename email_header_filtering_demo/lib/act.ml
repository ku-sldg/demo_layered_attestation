(* Copyright (c) 2025, The MITRE Corporation.
*)

let expand_config_file_name given =
  let given_dir = Filename.dirname given in
  let given_core = Filename.remove_extension
      (Filename.basename given) in
  let ext = Filename.extension given in
  let exec_name = Filename.remove_extension
      (Filename.basename Sys.argv.(0)) in
  let result = 
    Filename.concat
      (Set_up_env.maybe_get_env "CDS_CONFIG" given_dir)
      (exec_name ^ "_" ^ given_core ^ 
       (Set_up_env.maybe_get_env "CDS_CONFIG_MOD" "") ^ 
       ext) in
(*  print_endline given;
    print_endline result; *)
  result

let act src_dir_var dst_dir_var log_dir_var config_file_name 
    change_permitted =

  let _ = Set_up_env.set_dirs () in 
  let _ = Set_up_env.set_consts () in
  let filename = (Filename.basename Sys.argv.(1)) in
  let expand v modifier =
    Filename.concat (Sys.getenv v)
      (modifier ^ filename) in   

  try
    let in_file = expand src_dir_var "" in
    let out_file = expand dst_dir_var "" in
    let log_file =
      expand log_dir_var
        ((Filename.basename Sys.argv.(0)) ^ "_") in
    let (_,acts) =
      try 
        Read_config.interpret_config_file
          (expand_config_file_name config_file_name)          
      with
        e -> print_endline config_file_name;
        print_endline ("act:  Config file " ^
                       (Set_up_env.maybe_get_env
                          "CDS_CONFIG_FN" config_file_name) ^
                       " not found."); flush_all(); 
        raise e 
    in 
    flush_all(); 
    Pipeline.run_and_invoke
      (fun () -> 
         Pipeline.read_write_harness
           (fun in_file out_file log_file ->                
              (Header_fns.parse_transform_print_email_file
                 (Transforms.actions_transform_table acts) 
                 change_permitted
                 in_file out_file log_file))
           in_file out_file log_file)

  with
  | e ->
    print_endline (Sys.argv.(0) ^ ": " ^ filename); flush_all(); 
    raise e
