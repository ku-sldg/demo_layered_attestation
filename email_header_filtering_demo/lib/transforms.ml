(* Copyright (c) 2025, The MITRE Corporation.
*)

open Header_fns
open Config_types 


(*
let debug_chan = open_out (Filename.concat
                             (Sys.getenv "DEMO_ROOT")
                             (Filename.concat "external_arrivals" "tmp.out"))
*)


type field_contents =
  | Str of string               (* Verbatim or user name *)
  | Doms of string list         (* Address domains *)
  | Texts of string list        (* Address text fields *)
      
let contents_of_locale tbl = function
  | Header str ->
    let entries = hashtbl_entries tbl str in 
    List.map (function
        | Verbatim (_,c) -> Str c
        | Addresses (_,c,_) -> Str c)
      entries
  | Field (str,f) ->
    let entries = hashtbl_entries tbl str in
    List.concat_map (function
        | Verbatim _ ->
          failwith ("contents_of_locale:  No field "
                    ^ (string_of_fieldname f)
                    ^ " of verbatim entry for header "
                    ^ str)                                  
        | Addresses (_,_,afs) ->
          List.map
            (fun (Afield (_,u,ds,ts)) ->
               match f with
               | Username -> Str u
               | Domainnames -> Doms ds
               | Textfields -> Texts ts)
            afs)
      entries


let substring_at part whole start =
  let len_w = String.length whole in
  let len_p = String.length part in

  let rec iter i j =
    if j = len_p then true
    else if (try whole.[i+j] = part.[j] with
        | e -> print_endline
                 ("substring_at: " ^ (string_of_int i) ^
                  " in " ^ whole);
          raise e)
    then iter i (j+1)
    else false in 

  let rec trying i =
    if i+len_p > len_w then None
    else if iter i 0 then Some (i,i+len_p)
    else trying (i+1) in

  trying start 

(* Replace each occurrence of src by tgt in str.

   Notice, however, that occurrences may remain, if tgt contains them,
   or if tgt contains parts of occurrences of src that may adjoin the
   remainder of an occurrence in str.

*) 
let apply_string_subst src tgt str =
  let len_t = String.length tgt in
  let rec apply_from i so_far =
    match substring_at src so_far i with
    | None -> so_far
    | Some (j,k) ->
      apply_from (j+len_t)
        (try (String.sub so_far 0 j)
             ^ tgt
             ^ (String.sub so_far k ((String.length so_far)-k))
         with e -> print_endline
                     ("apply_string_subst: " ^ (string_of_int k) ^
                      " in " ^ so_far);
           raise e) in
  apply_from 0 str

let rec list_drop lst n =
  if n <= 0 then lst
  else match lst with
    | [] -> []
    | (_ :: rest) -> list_drop rest (n-1)

let rec starts_with_list = function
    | [] -> fun _ -> true
    | (a :: rest_part) -> function
      | [] -> false
      | (b :: _) when not(a = b) -> false 
      | (_ :: rest_whole) -> starts_with_list rest_part rest_whole

let rec apply_domain_subst src_list tgt_list domain_list =
  let starts = starts_with_list in
  if starts src_list domain_list
  then tgt_list
       @ (apply_domain_subst
            src_list tgt_list
            (list_drop domain_list (List.length src_list)))
  else match domain_list with
    | [] -> []
    | (a :: rest) -> a :: apply_domain_subst src_list
                       tgt_list rest

let rec contained_in_list part whole =
  starts_with_list part whole ||
  match whole with
  | [] -> false
  | _ :: rest -> contained_in_list part rest

let apply_header_subst = function
  | Strings (src,tgt) ->
    (function
      | Verbatim (i,body) ->
        Verbatim (i,apply_string_subst src tgt body)
      | Addresses (i,body,_) ->
        to_addresses i (apply_string_subst src tgt body))
  | Lists (_,_) ->
    (* no-op except on field,
       not whole header *)
    (fun c -> c)

let header_contains_src = function
  | Strings (src,_) -> (function 
      | Verbatim (_,body) -> contained_within src body 0        
      | Addresses (_,body,_) -> contained_within src body 0)    
  | Lists (_,_) -> (fun _ -> false)

let header_lacks_src subst entry =
  not (header_contains_src subst entry)

let header_lacks_str str = function 
  | Verbatim (_,body) -> not (contained_within str body 0) 
  | Addresses (_,body,_) -> not (contained_within str body 0)
  
        
let apply_header_update tbl hlabel =
  let entries = hashtbl_entries tbl hlabel in
  function
  | Require -> (match entries with
      | [] -> Failing
      | _ -> Unchanged) 
  | Prohibit -> (match entries with
      | [] -> Unchanged
      | _ -> hashtbl_record_entries tbl hlabel [];
        Changed)
  | Substitute subst ->
    let result =
      List.map
        (apply_header_subst subst)
        entries in
    if entries = result then Unchanged
    else 
      (hashtbl_record_entries tbl hlabel result;
       Changed)
  | Deletion str ->
    let result =
      List.filter (header_lacks_str str) entries in 
    if entries = result then Unchanged
    else
      (hashtbl_record_entries tbl hlabel result;
       Changed)
  | Insertion str ->
    let bodies = List.map (function
        | Verbatim (_,body) -> body
        | Addresses (_,body,_) -> body)
        entries in 
    if List.mem str bodies then Unchanged
    else
      let max_index = List.fold_left
          (fun so_far -> function 
             | Verbatim (i,_) -> max so_far i
             | Addresses (i,_,_) -> max so_far i)
          0 entries in 
      (hashtbl_cons_in_front tbl hlabel
         (Verbatim (max_index+1, str));
       Changed) 

let addr_texts_null =
  List.for_all
    (function Afield (_,_,_,ts) ->
       List.for_all (fun t -> (t = "")) ts)
    

let addr_usernames_null =
  List.for_all
    (function Afield (_,u,_,_) -> u = "")
    

let addr_domains_null =
  List.for_all
    (function
      | Afield (_,_,[],_) -> true
      | Afield (_,_,_,_) -> false)
    
(*  
  function
  | Verbatim (i,str)
    -> (fun _ _ -> (Unchanged, Verbatim(i,str)))
  (* failwith ("apply_field_update_to_entry: Verbatim "
                  ^ hlabel ^ ": " ^ str ^ " has no field "
                  ^ (string_of_fieldname fldname))
  *)
  | Addresses (i, body, afs) -> 
   *) 


let apply_field_update_to_entry = function
  | Require -> (function
      | Username -> (function
          | Addresses (i, body, afs) -> 
            if addr_usernames_null afs
            then (Failing, Addresses (i, body, afs))
            else (Unchanged, Addresses (i, body, afs))
          | v -> Unchanged, v)
      | Domainnames -> (function
          | Addresses (i, body, afs) -> 
            if addr_domains_null afs
            then (Failing, Addresses (i, body, afs))
            else (Unchanged, Addresses (i, body, afs))
          | v -> Unchanged, v)
      | Textfields -> (function
          | Addresses (i, body, afs) -> 
            if addr_texts_null afs
            then (Failing, Addresses (i, body, afs))
            else (Unchanged, Addresses (i, body, afs))
          | v -> Unchanged, v))
  | Prohibit -> (function
      | Textfields -> (function
          | Addresses (i, body, afs) -> 
            if addr_texts_null afs
            then (Unchanged, Addresses (i, body, afs))
            else
              let afs' =
                List.map
                  (function Afield(i,u,ds,_) ->
                     Afield(i,u,ds,[]))
                  afs in 
              (Changed, Addresses(i, format_subfields afs',
                                  afs'))
          | v -> Unchanged, v)
      | _ -> (function
          | Addresses (i, body, afs) ->
            (Unchanged, Addresses (i, body, afs))
          | v -> Unchanged, v))
           
  | Substitute (Strings (src, tgt)) -> (function
      | Username -> (function
          | Addresses (i, body, afs) -> 
            if List.exists
                (function Afield(_,u,_,_) ->
                   contained_within src u 0)
                afs
            then let afs' =
                   List.map (function Afield(i,u,ds,ts) ->
                       Afield (i,
                               (apply_string_subst src tgt u),
                               ds, ts))
                     afs in
              (Changed, Addresses(i, format_subfields afs',
                                  afs'))
            else (Unchanged, Addresses (i, body, afs))
          | v -> Unchanged,v)
      | Domainnames -> (function
          | Addresses (i, body, afs) -> 
            if List.exists
                (function Afield(_,_,ds,_) ->
                   List.exists
                     (fun d -> contained_within src d 0)
                     ds)
                afs
            then
              let afs' =
                List.map (function Afield(i,u,ds,ts) ->
                    Afield
                      (i,u, 
                       List.map (apply_string_subst src tgt) ds,
                       ts))
                  afs in
              (Changed, Addresses(i, format_subfields afs', afs'))
            else (Unchanged, Addresses (i, body, afs))
          | v -> Unchanged, v)
      | Textfields -> (function
          | Addresses (i, body, afs) -> 
            if List.exists
                (function Afield(_,_,_,ts) ->
                   List.exists
                     (fun t -> contained_within src t 0)
                     ts)
                afs
            then
              let afs' =
                List.map (function Afield(i,u,ds,ts) ->
                    Afield
                      (i,u, ds, 
                       List.map (apply_string_subst src tgt) ts))
                  afs in
              (Changed, Addresses(i, format_subfields afs', afs'))
            else (Unchanged, Addresses (i, body, afs))            
          | v -> Unchanged, v))
  | Substitute (Lists (srcs,tgts)) -> (function 
      | Domainnames -> (function
          | Addresses (i, body, afs) -> 
            if List.exists
                (function Afield (_,_,ds,_) ->
                   contained_in_list srcs ds)
                afs
            then
              let afs' = List.map (function Afield(i,u,ds,ts) ->
                  Afield
                    (i,u, 
                     apply_domain_subst srcs tgts ds,
                     ts))
                  afs in
              (Changed, Addresses(i, format_subfields afs', afs'))
            else (Unchanged, Addresses (i, body, afs))
          | v -> Unchanged, v)
      | _ -> (fun v -> Unchanged, v))
  | Deletion _ -> (fun _ v -> Unchanged, v)
  | Insertion _ -> (fun _ v -> Unchanged, v)

let apply_field_update tbl hlabel fld outcome =
  match fold_changes
          (apply_field_update_to_entry outcome fld)
          (hashtbl_entries tbl hlabel) with
  | Unchanged,_ -> Unchanged
  | Failing, _ -> Failing
  | Changed, es' -> hashtbl_record_entries tbl hlabel es'; Changed
    
let apply_update_to_locale tbl = function
  | Header hlabel -> apply_header_update tbl hlabel 
  | Field (hlabel,fld) -> apply_field_update tbl hlabel fld 
                
  
let apply_outcomes_to_locale tbl locale =
  fold_updates (apply_update_to_locale tbl locale)

let apply_action tbl (locales,outcomes) =
  fold_updates (fun l -> apply_outcomes_to_locale tbl l outcomes)
    locales

let apply_actions tbl =
  fold_updates (apply_action tbl)  
  
let actions_transform_table actions tbl =
  apply_actions tbl actions
  
  
    
  
