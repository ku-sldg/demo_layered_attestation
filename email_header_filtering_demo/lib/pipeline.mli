(* Copyright (c) 2025, The MITRE Corporation.
*)

val interpret_argv : unit -> (string * string array) option
val invoke_rest : string -> string array -> int
val run_and_invoke : (unit -> bool) -> unit
val read_write_harness :
  (string -> string -> string -> bool) -> string -> string -> string -> bool
val set_up_pipeline : string -> string -> int
val make_filename_generator : unit -> unit -> string
val open_append : string -> out_channel
val read_input_ch_to_file : in_channel -> string -> unit
val write_file_to_output_ch : string -> out_channel -> unit
val append_input_ch_to_file : in_channel -> string -> unit
val start_server : string -> string -> string -> int -> unit
val write_file_to_socket_addr : string -> Unix.sockaddr -> bool
val start_recipient_server : string -> string -> int -> unit
val email_msgs_in_dir : string -> string list
val transmit_msgs_in_dir : string -> Unix.sockaddr -> unit
