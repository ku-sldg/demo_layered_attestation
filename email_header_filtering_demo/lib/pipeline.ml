(* Copyright (c) 2025, The MITRE Corporation.
*)

(* The argument unix vector should have the current executable,
   followed by the filename to use as the input and output -- in the 
   appropriate directories -- and then a sequence of future
   executables that should be run afterwards.

   If the future sequence is empty, this process is the last of the
   line, so no more will need to be invoked.

   Otherwise, we will need to invoke the first in the sequence with
   the filename as argv.(1) and the remainder of the future
   executables.  The executable invoked will be argv.(0) as usual.
*)


let interpret_argv () =
  if Array.length Sys.argv < 3
  then
    None
  else
    let filename = Sys.argv.(1) in
    let exec_pathname = Sys.argv.(2) in
    let exec_rest =
      (* omit current executable, filename, next executable *) 
      List.tl (List.tl (List.tl (Array.to_list Sys.argv))) in 

    Some (exec_pathname,           (* the pathname to exec next *)
          Array.of_list
            (* put back next executable and filename
               in right places *) 
            (exec_pathname :: filename :: exec_rest))


let open_append =
  open_out_gen
    [Open_wronly; Open_creat; Open_append; Open_binary]
    0o666

let (read_input_ch_to_file,write_file_to_output_ch,
     append_input_ch_to_file,append_string_to_file) =
  let rec next_chunk inch outch buff =
    let count = input inch buff 0 128 in
    if count = 0
    then
      flush outch
    else
      (output outch buff 0 count;
       next_chunk inch outch buff) in
  
  (fun inch full_filename ->
     let outch = open_out full_filename in
     let buff = Bytes.create 256 in
     next_chunk inch outch buff;
     close_out outch),
  
  (fun full_filename outch -> 
     let inch = open_in full_filename in
     let buff = Bytes.create 256 in
     next_chunk inch outch buff; 
     close_in inch),

  (fun inch full_filename ->
     let outch = open_append full_filename in
     let buff = Bytes.create 256 in
     next_chunk inch outch buff;
     close_out outch),

  (fun full_filename str ->
     let outch = open_append full_filename in
     output_string outch str;
     close_out outch)


let invoke_rest ex args =
  (*  let std_in = Unix.openfile "/dev/null" [O_RDONLY] 0o640 in
      let std_out = Unix.openfile "/dev/null" [O_WRONLY] 0o640 in 
      let std_error = Unix.openfile "/dev/null" [O_WRONLY] 0o640 in
  *)
  Unix.create_process ex args Unix.stdin Unix.stdout
    Unix.stderr 
   
(* given a thunk *act*, invoke it, get the next executable and its
   argv, and create that process and exit.  It is expected to use the
   same run_and_invoke function to do its work and continue the
   pipeline.

   We enforce that none of these processes use std in, out, and error,
   since they should just use the file named by Sys.argv.(1) in the
   input and output directory.  
*)
      

let run_and_invoke (act : unit -> bool) =
  if act()
  then 
    match interpret_argv() with
    | None -> ()
    | Some (ex, args) -> ignore (invoke_rest ex args)
  else
    ()
    
let read_write_harness
    (act_on_files : string -> string -> string -> bool)
    in_file out_file log_file =    
  try
    if act_on_files in_file out_file log_file
    then 
      (Sys.remove in_file; true)
    else       
      (Sys.rename in_file (log_file ^ "_reject"); false)
  with _ ->
    flush_all(); 
    ignore(append_string_to_file log_file
             ("read_write_harness:  action failed on " ^
              in_file ^ "\n"));
    false 
    
  
(* append_input_ch_to_file (open_in in_file) log_file *) 

(* The pipeline of executables will be a :-separated path of 
   filenames.  We construct the argv of the first pipeline stage from
   this.  Each executable needs to know what environment variable to
   consult to get its input and output directory, as well as what
   act_on_files to use to generate output from input.  
*)

let set_up_pipeline pipeline_env_var filename =
  let executable_list =
    String.split_on_char ':' (Sys.getenv pipeline_env_var) in
  match executable_list with
  | [] -> failwith ("set_up_pipeline:  No executables in " ^
                    pipeline_env_var ^ " in process environment")
  | (first_executable :: rest) -> 
    invoke_rest first_executable
      (Array.of_list
         (first_executable :: filename :: rest))

let make_filename_generator () =
  let _ = Random.self_init () in 
  let next_random () =
    Int64.to_string (Random.int64 (Int64.max_int)) in
  fun () -> "fn_" ^ (next_random ()) ^ ".eml"

(* use this to build the input transfer receiver to get the msgs that
   will need to be transferred across the security boundary.  *) 

let start_server incoming_dir_env_var pipeline_env_var ip_addr port = 
  let socket_addr =
    Unix.ADDR_INET (Unix.inet_addr_of_string ip_addr, port) in
  Unix.establish_server
    (fun inch outch ->
       let get_new_filename = make_filename_generator () in 
       let filename = get_new_filename () in
       let full_path = 
         Filename.concat (Sys.getenv incoming_dir_env_var) filename in 
       read_input_ch_to_file inch full_path;
       close_out outch; 
       ignore (set_up_pipeline pipeline_env_var filename))
    socket_addr
   


(* use this when writing the output transfer process that sends a
   rewritten and filtered msg to the server at the far end.  
*)


let write_file_to_socket_addr full_filename socket_addr = 
  let (inch,outch) = Unix.open_connection socket_addr in
  write_file_to_output_ch full_filename outch;
  Unix.shutdown_connection inch;
  close_out outch;
  true


(* start a server at the far end to receive and record the rewritten 
   and filtered msgs.

   Just like start_server except (1) no need to retrieve a directory
   from an environment variable, and (2) no need to start yet another
   pipeline; we got there!
*)


let start_recipient_server target_dir ip_addr port =
  let socket_addr =
    Unix.ADDR_INET (Unix.inet_addr_of_string ip_addr, port) in
  Unix.establish_server
    (fun inch outch ->
       let get_new_filename = make_filename_generator () in
       let filename = get_new_filename () in
       let full_path = 
         Filename.concat target_dir filename in 
       read_input_ch_to_file inch full_path;
       close_out outch)
    socket_addr

let email_msgs_in_dir dir =
  let d = Unix.opendir dir in
  let rec collect so_far =
    try
      let next = Unix.readdir d in    
      if ".eml" = Filename.extension next
      then
        collect (next :: so_far)
      else
        collect so_far
    with
    | End_of_file -> so_far
  in
  collect [] 

let transmit_msgs_in_dir dir peer_sock_addr =
  List.iter
    (fun fn ->
       ignore(write_file_to_socket_addr
                (Filename.concat dir fn)
                peer_sock_addr))
    (email_msgs_in_dir dir)
    
