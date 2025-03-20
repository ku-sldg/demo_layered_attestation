(* Copyright (c) 2025, The MITRE Corporation.
*)

type scan_pts = {
  commas : int list;
  at_signs : int list;
  protected_regions : (int * int) list;
}

val scan_address_field : string -> int -> int -> scan_pts option
