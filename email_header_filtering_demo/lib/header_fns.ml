(* Copyright (c) 2025, The MITRE Corporation.
*)

(* exception for error reporting.
   Catch when evaluating msg file.
*) 

exception Field_error of string * int * string

(* raise an error in case of mis-parsing... *) 

let report_parse_error str i =
  raise (Field_error ("report_parse_error at pos", i, str))

(* Several auxiliaries for breaking the heading of a msg file into headers.  *)  

let is_empty str =
  str = "" || str = "\r" || str = "\r\n" 

let is_continuation_line str =
  not(is_empty str) &&
  (str.[0] = ' ' || str.[0] = '\t')
  
let begins_header str =
  not(is_empty str) &&
  not(is_continuation_line str)

let header_name str =
  match String.index_from_opt str 0 ':' with
  | None -> None
  | Some i -> Some (String.sub str 0 i)

let split_at str i skip =
  let j = i+skip in 
  (String.sub str 0 i,
   String.sub str j ((String.length str)-j))

(* Skip one character to discard the ':'
   which is no part of the contents or the 
   header. *) 

let header_name_and_contents str =
  match String.index_from_opt str 0 ':' with
  | None -> None
  | Some i -> Some (split_at str i 1)
                    

let does_header_begin str =
  begins_header str &&
  match header_name str with
  | None -> false
  | Some _ -> true

(* lines is the backwards list of header lines that have been read since the
   start or the last header was completed.

   The header result consists of the concatenation in the reversed (forwards) 
   order, with whitespace where the line breaks were.
*) 
let header_of_lines lines =
  String.concat "\n" (List.rev lines)

(* the string that gloms together the chars
   in a char list *) 

let string_of_list l =
  try String.init (List.length l) (fun i -> List.nth l i)
  with e -> print_endline "yikes"; raise e 
    
    
    
(* to package up a result, we
   reverse it before string_of_list
   and remember the resulting index.
*) 

let result_of_so_far i so_far =
  i, string_of_list (List.rev so_far)

(* auxiliary to compute if *part*
   appears in *whole* starting at *index*, 
   without extending beyond *stop*. 
*) 

let contained_at part whole index stop =
  let part_len = String.length part in 
  (String.length whole) >= stop && 
  stop >= index+part_len &&
  let rec run i =
    i = part_len ||
    (part.[i] = whole.[index+i] && run (i+1)) in 
  run 0

(* does *part* appear anywhere in *whole*
   between *start* and *stop*?  *)

let contained_within part whole start  =
  let part_len = String.length part in
  let whole_len = String.length whole in 
  let rec iter i =
    contained_at part whole i whole_len ||
    (i+part_len < whole_len &&
     iter (i+1)) in
  iter start 
    


let is_user_name_char ch =
  ('A' <= ch && ch <= 'z') ||
  ('0' <= ch && ch <= '9') ||
  ch = '.' || ch = '_' || ch = '-' || ch = '+' || ch = '='

let is_domain_name_char ch =
  ('A' <= ch && ch <= 'z') ||
  ('0' <= ch && ch <= '9') ||
  ch = '_' || ch = '-'

(* grow_back and grow_forward do *not* recognize
   escape characters.  I think that's right for
   usernames and domain components.
*)
      
let grow_back allowed_p str at_pos lower_bound =
  let rec iter i so_far =
    if i < lower_bound
    then                        (* don't reverse *)
      (i+1, string_of_list so_far) 
    else
      let prev_ch =
        (try str.[i] with
         | e -> print_endline
                  ("grow_back: " ^ (string_of_int i) ^
                   " in " ^ str);
           raise e) in
      if not(allowed_p prev_ch)
      then                      (* reached boundary *)
        (i+1, string_of_list so_far)
      else                      (* collect char *)
        iter (i-1)
          (prev_ch :: so_far)   (* collecting in right order *)
  in
  iter (at_pos-1) []

let grow_user_name_back = grow_back is_user_name_char

let grow_forward allowed_p str at_pos upper_bound =
  let rec iter i chars_so_far domains_so_far =
    if i >= upper_bound
    then
      (i,
       (List.rev
          ((string_of_list (List.rev chars_so_far))
           :: domains_so_far)))
    else
      let next_ch = (try str.[i] with
          | e -> print_endline
                   ("grow_forward: " ^ (string_of_int i) ^
                    " in " ^ str);
            raise e) in
      if next_ch = '.'
      then
        iter (i+1) []
          ((string_of_list (List.rev chars_so_far))
           :: domains_so_far)
      else if not(allowed_p next_ch)
      then
        (i,
         (List.rev
            ((string_of_list (List.rev chars_so_far))
             :: domains_so_far)))
      else
        iter (i+1) (next_ch :: chars_so_far) domains_so_far in
  iter (at_pos-1) [] [] 
          

let grow_domains_forward = grow_forward is_domain_name_char

(* Given

   a string str,
   a list of indices at which we have commas, and
   a list of indices at which we have at signs @,
   
   return a list of
   (start, i, j, end, name, domain list) tuples
   such that the field begins at start and ends at end;
   the name gets done at the @ and begins at i, and the
   domain list begins after the @ and gets done at j.

   For instance, if the string is

   "guttman@mitre.org (Joshua D. Guttman), Joshua Guttman <joshua.guttman@gmail.com>"

   and the cms are [37] and the ats are [7; 69], then we want

   [(0, 0, 17, 37, "guttman", ["mitre"; "org"]);
    (38, 55, 79, 80, "joshua.guttman", ["gmail"; "com"])]

   since the first address stretches from 0 to 17 and the second from
   character 55 to 79.

   We require that an address that starts with '<" *or* ends with '>'
   must start with '<" *and* end with '>'.  
   
   *)

let username_domains_from_scan_pts str cms ats =
  (* start at 0 and
     immediately after each comma *) 
  let starts = 0 :: List.map (fun i -> i+1) cms in
  (* end at each comma and
     at the string length *) 
  let stops = cms @ [String.length str] in

  let brackets_balance i j =
    (str.[i]  = '<' && str.[j]  = '>') ||
    (str.[i] != '<' && str.[j] != '>') in

  let next_username_domains start at stop =
    (*     print_int start ; print_char ' ';
           print_int stop; print_char ' ';
           print_newline (); 
           print_newline ();
    *) 
    let (i, n) = grow_user_name_back str at start in
    let (j, ds) = grow_domains_forward str (at+2) stop in
    if brackets_balance (i-1) j
    then (start, i, j, stop, n, ds)
    else
      failwith (
        "brackets_balance:  Mismatch at positions " ^ 
        (string_of_int i) ^ ", " ^ (string_of_int j) ^
        " in " ^ str)   in
  
  List.mapi
    (fun i ati ->
       try next_username_domains (List.nth starts i) ati
             (List.nth stops i)
       with | e ->
         (print_endline
            ("next_username_domains: " ^
             (string_of_int i) ^ " reached length of " ^
             (String.concat ", " (List.map string_of_int starts)) ^
             " or " ^ (String.concat ", "
                         (List.map string_of_int stops))); 
          raise e)) 
    ats
      
(* Does a character appear in a string,
   and in which position?
*) 

let within_string ch str =
  let len = String.length str in 
  let rec search i =
    if i >= len then None  
    else if ch = str.[i] then Some i 
    else search (i+1) in
  search 0

let whitespace_chars = " \t\n\r"

(* Is a character a whitespace character? *) 

let char_is_whitespace ch =
  match within_string ch whitespace_chars with
  | Some _ -> true
  | None -> false 

(* Advance past all adjacent whitespace.  *) 
          
let skip_whitespace str i stop =
  let rec skip j =
    if j=stop then j
    else if char_is_whitespace str.[j]
    then skip (j+1)
    else j
  in skip i

(* Accumulate the sum of the string lengths so far in the headers.
   Add 1 each time, since reading the lines omits the linefeed that
   separates any two headers.  
*)
    
let indices_of_headers headers =
  (List.rev
     (List.fold_left
        (function
          | [] -> fun _ ->
            failwith "indices_of_headers:  Can't be empty" 
          | (i :: rest) -> fun str
            -> (String.length str)+1+i :: (i :: rest))
        [0]
        headers))
    
(* Construct a list of all of the headers read from file fn,
   compute their successive start indices, and then construct a list
   of all of the body lines of the file.  Return the three lists of
   header strings, header indices, and body lines.  
*)
  
let read_email_file fn =
  let ch = try open_in fn
    with
      e -> (print_endline
              ("read_email_file:  didn't find input file " ^
               fn);
            raise e) 
  in 
  let rec read_next_header_line (current_lines, header_list) =
    let l = input_line ch in
    if is_empty l 
    then
      (header_of_lines current_lines) :: header_list
    else if is_continuation_line l
    then
      read_next_header_line
        (l :: current_lines, header_list)
    else if begins_header l
    then
      read_next_header_line
        ([l],
         (header_of_lines current_lines) :: header_list)
    else failwith "Huh?" in

  let try_input_one_line ch =
    try
      Some (input_line ch)
    with
      End_of_file -> None in 

  let rec read_body_lines so_far =
    match try_input_one_line ch with
    | None -> so_far
    | Some l -> read_body_lines (l :: so_far) in         
  
  let l = input_line ch in
  if begins_header l
  then 
    let headers = List.rev (read_next_header_line ([l],[])) in
    let indices = indices_of_headers headers in 
    let body_lines = List.rev (read_body_lines []) in
    (headers,
     indices,
     body_lines)
  else failwith "Bad file does not start with a header line"

(* Header names agree if they are the same modulo case.  *)  

let header_names_agree h1 h2 =
  String.lowercase_ascii h1 =
  String.lowercase_ascii h2

(* Each address has
       an offset, 
       a username,
       a list of domain components, and
       a list of text regions that may give the recipient's name
  *) 

type address_field =
    Afield of
      (int                     (* subfield start relative to whole field *)
       * string                (* user name *)
       * string list           (* domain name list *) 
       * string list)          (* text fields *)
   
(* Each table entry will have an int that
   reports its position in the original msg.

   After filtering and rewriting, we will
   preserve order when emitting them.     
*)

type header_table_entry =
  | Verbatim of (int * string)  (* start and contents *)
  
  | Addresses of
      (int                      (* start of the field *)
       * string                 (* whole header contents *)
       * address_field list)    (* subfields, each an Afield *)

let format_subfield (Afield (_, name, domains, texts)) =
  let left_embrace left = if left.[(String.length left)-1] = '<'
    then left 
    else left ^ "<" in
  let right_embrace right =
    if (String.length right) > 0 && right.[0] = '>'
    then right 
    else ">" ^ right in 
    
  let assemble domains = (String.concat "." domains) in 
  match texts with
  | [] ->
    Printf.sprintf "<%s@%s>"
      name (assemble domains)
  | [only] ->
    Printf.sprintf "%s%s@%s>"
      (left_embrace only)
      name (assemble domains)
  | (pre :: rest) ->
    Printf.sprintf "%s%s@%s%s"
      (left_embrace pre)
      name (assemble domains)
      (right_embrace (String.concat "" rest))
                    
let format_subfields subfields =
  (* format addresses one per line *)
  String.concat ",\r\n  "   
    (List.map format_subfield (List.rev subfields))

(* is any other kind of entry needed here?
   Let's see! *)


let hashtbl_entries tbl key =
   match Hashtbl.find_opt tbl key with
    | None -> []
    | Some l -> l

(*
let hashtbl_cons_in_front = Hashtbl.add 
*)

let hashtbl_cons_in_front tbl key new_value =
  let old_list = hashtbl_entries tbl key in
  Hashtbl.replace tbl key (new_value :: old_list)

let hashtbl_record_entries = Hashtbl.replace

let to_verbatim i str = Verbatim (i,str)

let to_addresses offset str =
  let addressee_entry (start, i, j, stop, name, domains) =    
    Afield (i, name, domains,
            [String.sub str start (i-start);
             String.sub str j (stop-j)])
  in 

  match Scan_field.scan_address_field
          str 0 
          (String.length str) with
  | None -> failwith("to_addresses:  Scan_field.scan_address_field" ^ 
                     " reported failure for" ^
                     str)
  | Some scan_pts  ->     
    let uds =
      username_domains_from_scan_pts str
        scan_pts.commas scan_pts.at_signs in 
    Addresses
      (offset,
       str,
       (List.map addressee_entry uds))
    
let header_parsers = [
  ("subject", to_verbatim);
  ("to", to_addresses);
  ("cc", to_addresses);
  ("bcc", to_addresses); 
  ("from", to_addresses); 
  ("reply-to", to_addresses);
  ("return-path", to_addresses)
]

let rec all_but_last = function
  | [] -> []                    (* failing seems too harsh!  Was 
                                   failwith "all_but_last:  Null arg"
                                *) 
  | (_ :: []) -> []             (* omit last *)
  | (a :: rest) ->              (* don't omit non-last *)
    a :: (all_but_last rest)


let register_headers headers indices =
  let starts = all_but_last indices in 
  let tbl = Hashtbl.create 100 in

  let hparser hlabel =
    match List.assoc_opt (String.lowercase_ascii hlabel)
            header_parsers with
    | None -> (fun index a -> Verbatim (index,a))
    | Some f -> f in 

  let add_header_entry h index =
    match header_name_and_contents h with
    | None -> failwith "Bad file has a header with no header_name"
    | Some (hlabel,hcontents) ->
      hashtbl_cons_in_front tbl hlabel
        ((hparser hlabel)
           (index+(String.length hlabel)+1)
           hcontents) in

  List.iter2 add_header_entry headers starts;
  tbl

let indexed_string_of_binding hlabel = function
  | Verbatim (i,hcontents) ->
    (* hcontents should contain
       its terminating carriage return *) 
    i, Printf.sprintf "%s: %s\n" hlabel hcontents 
  | Addresses 
      (i, orig_hcontents, subfields) ->     
    if not(orig_hcontents = "")
    then i, Printf.sprintf "%s: %s\n" hlabel orig_hcontents
    else
      i, Printf.sprintf "%s: %s\r\n" hlabel
        (format_subfields subfields)

let write_table out_ch header_tbl =
  let index_strings = 
    Hashtbl.fold (fun hl hcs so_far -> 
        (List.map (indexed_string_of_binding hl) hcs) @ so_far)
      header_tbl [] in
  List.iter
    (fun (_,s) ->
       output_string out_ch s
       (*;
         output_char out_ch '\n'*)
    )
    (List.sort
       (fun (i,_) (j,_) -> i-j)
       index_strings)
  
let parse_email_file in_file =
  let (headers,indices,body_lines) = read_email_file in_file in
  let header_tbl = register_headers headers indices in
  (header_tbl, body_lines)

let parse_print_email_file in_file out_file =
  let (header_tbl, body_lines) = parse_email_file in_file in
  let out_ch = open_out out_file in
  
  write_table out_ch header_tbl;
  output_string out_ch "\r\n";
  List.iter
    (fun l ->
       output_string out_ch l;
       output_char out_ch '\010')
    body_lines
    
let parse_transform_print_email_file transform permit_change
  in_file out_file log_file = 
  let (header_tbl, body_lines) = parse_email_file in_file in
  let err_ch = open_out log_file in
  output_string err_ch (in_file ^ "\n"); flush_all () ;
  let update = transform header_tbl in 
  let result =
    Config_types.did_we_succeed permit_change update in  
  if result
  then
    (let out_ch = open_out out_file in 
     write_table out_ch header_tbl;
     output_string out_ch "\r\n";
     List.iter
       (fun l ->
          output_string out_ch l;
          output_char out_ch '\010')
       body_lines;
     close_out out_ch;
     match update with
     | Changed ->      
       (if not(Sys.file_exists in_file)
        then print_endline ("Diffing:  No such input file "
                            ^ in_file)
        else if not(Sys.file_exists out_file)
        then print_endline ("Diffing:  No such output file "
                            ^ out_file)
        else
          (print_endline "doing diff"; 
           ignore(Unix.system
                    ((Unix.getenv "DIFF_PROG")
                     ^ " -c -w " ^ in_file ^
                     " " ^ out_file ^ " > " ^
                     (log_file ^ ".diff"))); 
           print_endline "did diff");
        result)      
     | _ -> result)
  else
    (output_string err_ch
       ("Failed on input file " ^ in_file
       (*        ^ ", contents follow\n" *)
       );
     output_string err_ch "\n\n";
     flush_all(); 
     result)




    



