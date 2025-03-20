(* Copyright (c) 2025, The MITRE Corporation.
*)

type scan_pts = {
  commas : int list ;
  at_signs : int list ;
  protected_regions : (int * int) list
}

let reverse_scan_pts record =
  { commas = List.rev record.commas ; 
    at_signs = List.rev record.at_signs ;
    protected_regions = List.rev record.protected_regions
  }


let check_one_address_per_comma_delimited_region record =

  (* iter takes pairs of the form 

       (at-list,comma-list)
     
  *) 

  let rec iter = function    
    | (_ :: [], []) -> true     (* only at sign *)
    | (_, []) -> false          (* not enough commas *)
    | ([], _ :: _) -> false     (* too many commas *)
    | ((a :: at_rest), (c :: comma_rest))
      -> a<c &&
         match at_rest with
         | a1 :: _ -> c<a1 && iter (at_rest, comma_rest)
         | [] -> false          (* too many commas,
                                   but iter would apply case 3 *)
  in

  match (record.at_signs, record.commas) with
  | ([],[]) -> true
  | (ats,cms) -> iter (ats,cms)

let protecting_delimiters = [ ('"', '"'); ('(', ')') ]
                            
let protecting_starters = List.map fst protecting_delimiters

let unprotector_of char =
  match List.assoc_opt char protecting_delimiters with
    None -> '"'
  | Some ch -> ch

let escape_char = '\\'                 

let scan_address_field str start stop =
  let rec next_occurrence ch i =
    if i >= stop then None
    else
      let next_ch =
        (try str.[i]
         with e -> print_endline ("next_occurrence: " ^ str);
           raise e) in
      if next_ch = escape_char
      then                      (* skip both *)
        next_occurrence ch (i+2)
      else if next_ch = ch
      then                      (* found *)
        Some i
      else if List.mem next_ch protecting_starters
      then
        match next_occurrence
                (unprotector_of next_ch) (i+1) with
        | None -> None
        | Some j -> next_occurrence ch (j+1)
      else                       (* keep going *)
        next_occurrence ch (i+1) in 
    
  let rec scan i record =
    if i >= stop
    then (let rev_rec = reverse_scan_pts record in
          if check_one_address_per_comma_delimited_region rev_rec
          then Some rev_rec
          else None)
    else
      let ch = (try str.[i]
         with e -> print_endline ("scan_address_field: " ^ str);
           raise e) in
      if ch = '@'
      then scan (i+1)
          { record with
            at_signs = i :: record.at_signs }
      else if ch = ','
      then scan (i+1)
          { record with
            commas = i :: record.commas }
      else if List.mem ch protecting_starters
      then match next_occurrence
                   (unprotector_of ch) (i+1) with
        | None -> None
        | Some j -> scan (j+1)
                      { record with
                        protected_regions =
                          ((i,j) :: record.protected_regions) }       
      else scan (i+1) record
  in
  let record = { commas = [] ;
                 at_signs = [] ;
                 protected_regions = [] } in
  scan start record 

