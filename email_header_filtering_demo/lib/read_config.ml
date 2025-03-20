(* Copyright (c) 2025, The MITRE Corporation.
*)

open Config_types 

type defn_table =
  { string_defns :
      (string,                (* definiendum *)
       string list)           (* definiens *)                   
        Hashtbl.t ;
    domain_defns :
      (string,                (* definiendum *)
       string list            (* domain lists *)
         list)                (* jointly the definiens *)                   
        Hashtbl.t ;
    substn_defns :
      (string,                (* definiendum *)
       (string list           (* subst sources *)
        * string)             (* subst target to replace them *)
         list)                (* jointly the definiens *)                   
        Hashtbl.t ;
    domain_substn_defns :
      (string,                (* definiendum *)
       (string list list      (* subst sources *)
        * string list)        (* subst target to replace them *)
         list)
      Hashtbl.t
  }

let init_defn_table () =
  { string_defns = Hashtbl.create 79 ;
    domain_defns = Hashtbl.create 79 ;
    substn_defns = Hashtbl.create 79 ;
    domain_substn_defns = Hashtbl.create 79 }

let string_of_dom_subst (src_lists, dst_list) =
  "[ " ^ (String.concat "; "
            (List.map string_of_domains src_lists)) ^
  " --> " ^ (string_of_domains dst_list) ^          
  " ]"

let string_of_dom_subst_entry (def, substs) =
  "< " ^ def ^ " => " ^
  (String.concat "; "
     (List.map string_of_dom_subst substs)) ^
  " > " 
    


(* We expect the json object j to be either a new defn or else
   an action.  In the first case, interpret_json expands the
   defn using what's in the defn table, and then side-effects
   the table, returning Defn.

   In the second case, we expand the json via the defns in the
   defn table, and we then return Action act where act is the
   resulting action.  
*)

let label_appears lab = function
  | `Assoc (pairs) ->
    (match List.assoc_opt lab pairs with
     | Some _ -> true 
     | None -> false)
  | _ -> false

let val_of_label lab = function
  | `Assoc (pairs) ->
    (match List.assoc_opt lab pairs with
     | Some v -> v 
     | None -> failwith
                 ("val_of_label:  Label " ^ lab ^
                  " not found in input " ^
                  (Yojson.Basic.pretty_to_string (`Assoc (pairs)))))
  | j -> failwith
           ("val_of_label:  Label " ^ lab ^
            " not found in input " ^
            (Yojson.Basic.pretty_to_string j))

let string_of_label lab j =
  match val_of_label lab j with
  | `String str -> str
  | v -> failwith
           ("string_of_label: Non-string json encountered " ^
            (Yojson.Basic.pretty_to_string v))

let strings_of_vector = function
  | `List js ->
    List.map
      (function | `String str -> str
                | j -> failwith
                         ("strings_of_vector:  String " ^
                          "expected, found " ^
                          (Yojson.Basic.pretty_to_string j))) 
      js
  | j -> failwith ("strings_of_vector:  Vector of strings "
                   ^ "expected, found "
                   ^ (Yojson.Basic.pretty_to_string j))

let is_header_locale j =
  label_appears "header" j &&
  not(label_appears "field" j)

let is_field_locale j =
  label_appears "header" j &&
  label_appears "field" j

let is_domains_locale j =
  is_field_locale j &&
  match val_of_label "field" j with
  | `String "domain" -> true
  | _ -> false 

let is_action j =
  label_appears "locale" j
  && label_appears "outcome" j

let interpret_string dt str =
  match Hashtbl.find_opt dt.string_defns str with
  | None -> [str]
  | Some strs -> strs

let interpret_field_name dt str =
  List.map
    (function
      | "user" -> Username
      | "domain" -> Domainnames
      | "text" -> Textfields
      | fn -> failwith
                ("interpret_field_name:  Unknown field name " ^ fn))
    (match Hashtbl.find_opt dt.string_defns str with
     | None -> [str]
     | Some strs -> strs)

let interpret_locale dt = function
  | `String str -> List.map (fun v -> Header v)
                     (interpret_string dt str)
  | j ->
    if is_field_locale j
    then
      List.concat_map
        (fun f ->
           (List.map
              (fun h -> Field (h, f))
              (interpret_string dt (string_of_label "header" j))))
        (interpret_field_name dt (string_of_label "field" j))
    else
      failwith ("interpret_locale:  Non-locale " ^
                (Yojson.Basic.pretty_to_string j))

let rec cartesian_product = function
  | [] -> [[]]
  | l :: rest ->
    let partial = cartesian_product rest in
    List.concat_map
      (fun a -> List.map (fun part -> a :: part) partial)
      l

let rec thread = function
  | [] -> [[]]
  | l :: rest ->
    let partial = thread rest in
    List.concat_map
      (fun a ->
         List.map
           (fun p -> a @ p)
           partial)
      l

let interpret_domains_entry dt = function
  | `String str ->
    (match Hashtbl.find_opt dt.domain_defns str with
     | None -> [[str]]
     | Some string_lists -> string_lists)
  | `List js ->
    cartesian_product
      (List.map
         (function
           | `String dom_name -> interpret_string dt dom_name
           | j -> failwith
                    ("interpret_domains_entry: ill-formed domain spec "
                     ^ (Yojson.Basic.pretty_to_string j)))
         js)
  | j -> failwith ("interpret_domains_entry: ill-formed domain spec "
                   ^ (Yojson.Basic.pretty_to_string j))

let interpret_domains_literally = function
  | `List js ->
    List.map
      (function
        | `String dom_name -> dom_name
        | j ->
          failwith
            ("interpret_domains_literally: " ^
             "not a literal domain name "  ^
             (Yojson.Basic.pretty_to_string j)))
      js
  | j -> failwith ("interpret_domains_literally: " ^
                   "not a literal domain name " ^
                   (Yojson.Basic.pretty_to_string j))
  

(* in this and the rest of the treatment of substs, the *from*
   json object may expand to a whole family.  However, the
   substs that result must act like a function, not like a
   relation.  Thus, the *to* json object should be regarded
   literally as a single result to be substituted in place of
   any of the *from* values.

   Thus there's an important asymmetry in that the *from* is
   expanded while the *to* isn't.  *)

let interpret_domain_subst dt = function
  | `String str ->
    (match Hashtbl.find_opt dt.domain_substn_defns str with
     | None -> failwith("interpret_domain_subst:  domain defn " ^
                        str ^ "not found")
     | Some defns ->
       List.concat_map
         (fun (src_doms_list, tgt_doms) ->
            List.map (fun src_doms -> Lists (src_doms,tgt_doms))
              src_doms_list)
         defns)
  | `Assoc _ as j ->   
    let from_j = (val_of_label "from" j) in
    let to_j = (val_of_label "to" j) in 
    let target = interpret_domains_literally to_j in
    List.map
      (fun src -> Lists (src,target))
      (interpret_domains_entry dt from_j)
  | j -> failwith("interpret_domain_subst:  domain defn " ^
                        (Yojson.Basic.pretty_to_string j) ^ " ill-formed")

let str_subst_of_strings src_list tgt =
   List.map
     (fun src -> Strings (src,tgt))
     src_list

let strings_of_subst_immediate dt j =
  if label_appears "to" j && label_appears "from" j
    then
        (interpret_string dt (string_of_label "from" j), 
         string_of_label "to" j)
    else
      failwith ("subst_of_immediate:  Non-subst " ^
                (Yojson.Basic.pretty_to_string j))

let subst_of_immediate dt j =
  let (srcs,tgt) = strings_of_subst_immediate dt j in
  str_subst_of_strings srcs tgt

let interpret_str_subst dt = function
  | `String str ->
    (match Hashtbl.find_opt dt.substn_defns str with
    | None -> failwith ("interpret_str_subst:  Non-subst " ^ str) 
    | Some substs ->
      List.concat_map 
        (fun (src_list,tgt) -> str_subst_of_strings src_list tgt)
        substs)
  | j -> subst_of_immediate dt j
        
let interpret_outcome dt locale_is_domain = function
  | `String "require" -> [Require]
  | `String "prohibit" -> [Prohibit]
  | j ->
    if label_appears "delete-if" j
    then
      List.map
        (fun str -> Deletion str)
        (interpret_string dt (string_of_label "delete-if" j))
    else if label_appears "insert" j
    then
      List.map
        (fun str -> Insertion str)
        (interpret_string dt (string_of_label "insert" j))
    else if not(locale_is_domain)
    then
      List.map
        (fun str -> Substitute str)
        (interpret_str_subst dt j)
    else if locale_is_domain 
    then
      List.map
        (fun str -> Substitute str)
        (interpret_domain_subst dt j)
(*           (val_of_label "from" j)
           (val_of_label "to" j)
*)
    else 
      failwith ("interpret_outcome: Not an outcome " ^
                (Yojson.Basic.pretty_to_string j))
(*  | j -> failwith ("interpret_outcome: Not an outcome " ^
                   (Yojson.Basic.pretty_to_string j))
*)


let interpret_action dt j =
  let locale = val_of_label "locale" j in 
  let locale_is_domain = is_domains_locale locale in
  (interpret_locale dt locale,
   interpret_outcome dt locale_is_domain 
     (val_of_label "outcome" j)) 

let is_str_defn = label_appears "declare-str"
let str_defn_body = val_of_label "declare-str"
    
let is_doms_defn = label_appears "declare-doms"
let doms_defn_body = val_of_label "declare-doms"

let is_str_subst_defn = label_appears "declare-str-subst"
let str_subst_defn_body = val_of_label "declare-str-subst"

let is_doms_subst_defn = label_appears "declare-doms-subst"
let doms_subst_defn_body = val_of_label "declare-doms-subst"

let interpret_str_defn_pair dt defs tgt  =
  match Hashtbl.find_opt dt.string_defns defs with
  |  Some _ -> failwith ("interpret_str_defn_pair: already defined " ^ 
                         defs)
  | None ->
    (Hashtbl.add dt.string_defns defs
       (List.concat_map
          (interpret_string dt) 
          (match tgt with
           | `String str -> [str]
           | `List _ as v -> strings_of_vector v
           | j ->
             failwith ("interpret_str_defn_pair:  ill-formed defn " ^
                       (Yojson.Basic.pretty_to_string j)))))


let interpret_str_defn dt = function
  | `Assoc pairs ->
    List.iter
      (fun (defs, tgt) ->
         interpret_str_defn_pair dt defs tgt)
      pairs
  | j ->
    failwith ("interpret_str_defn:  ill-formed defn " ^
              (Yojson.Basic.pretty_to_string j))

let interpret_doms_defn_pair dt defs tgt =
  match Hashtbl.find_opt dt.domain_defns defs with
  | Some _ -> failwith ("interpret_doms_defn: already defined " ^ defs) 
  | None ->
    Hashtbl.add dt.domain_defns defs
      (match tgt with
       | `List v ->
         (List.concat_map
            (function
              | `List doms ->
                thread
                  (List.map (interpret_domains_entry dt)
                     doms)
              | j ->
                failwith ("interpret_doms_defn_pair:  ill-formed vector " ^
                          (Yojson.Basic.pretty_to_string j)))
            v)
       | j ->
         failwith ("interpret_doms_defn_pair:  ill-formed defn " ^
                   (Yojson.Basic.pretty_to_string j)))


let interpret_doms_defn dt = function
  | `Assoc pairs ->
    List.iter
      (fun (defs, tgt) ->
         interpret_doms_defn_pair dt defs tgt)
      pairs
  | j ->
    failwith ("interpret_doms_defn:  ill-formed defn " ^
              (Yojson.Basic.pretty_to_string j))


let interpret_str_subst_defn_pair dt defs tgt =
  match Hashtbl.find_opt dt.substn_defns defs with
  | Some _ -> failwith ("interpret_subst_defn: already defined "
                        ^ defs)
  | None -> 
    Hashtbl.add dt.substn_defns defs
      (match tgt with
       | `Assoc _ as j -> [strings_of_subst_immediate dt j] 
       | `List js -> List.map (strings_of_subst_immediate dt) js
       | j -> failwith
                ("interpret_str_subst_defn_pair:  ill-formed defn " ^
                 (Yojson.Basic.pretty_to_string j)))    

let interpret_str_subst_defn dt = function
  | `Assoc pairs ->
    List.iter
      (fun (defs, tgt) ->
         interpret_str_subst_defn_pair dt defs tgt)
      pairs
  | j -> failwith ("interpret_str_subst_defn:  ill-formed defn " ^
                   (Yojson.Basic.pretty_to_string j))

let interpret_doms_subst_defn_pair dt defs tgt =
  match Hashtbl.find_opt dt.domain_substn_defns defs with
  | Some _ -> failwith ("interpret_doms_subst_defn: already defined " ^ defs)
  | None ->
    let do_one j =
      let sources = interpret_domains_entry dt
          (val_of_label "from" j) in
      let dsts = interpret_domains_entry dt
          (val_of_label "to" j) in
      List.map (fun dst -> (sources,dst))
        dsts in 
    Hashtbl.add dt.domain_substn_defns defs
      (match tgt with
       | `Assoc _ as j -> do_one j         
       | `List v ->
         List.concat_map do_one v 
       | j ->
         failwith
           ("interpret_doms_subst_defn_pair:  ill-formed defn" ^
              (Yojson.Basic.pretty_to_string j))) 
         
let rec interpret_doms_subst_defn dt = function
  | `Assoc pairs ->
    List.iter
      (fun (defs, tgt) ->
         interpret_doms_subst_defn_pair dt defs tgt)
      pairs
  | `List v ->
    List.iter
      (fun j ->
         interpret_doms_subst_defn dt j)
      v
  | j -> failwith ("interpret_doms_subst_defn:  ill-formed defn " ^
                   (Yojson.Basic.pretty_to_string j))

let interpret_json dt j =
  if is_action j
  then interpret_action dt j
  else (if is_str_defn j
        then interpret_str_defn dt (str_defn_body j)
        else if is_doms_defn j
        then interpret_doms_defn dt (doms_defn_body j)
        else if is_str_subst_defn j
        then interpret_str_subst_defn dt (str_subst_defn_body j)
        else if is_doms_subst_defn j
        then interpret_doms_subst_defn dt (doms_subst_defn_body j)
        else failwith ("interpret_json:  Bad json input " ^
                       (Yojson.Basic.pretty_to_string j));
        ([],[]))

let interpret_config_file fn =
  let dt = init_defn_table () in
  let acts =
    List.rev
      (Seq.fold_left
         (fun actions j ->
            let (locales,outcomes) = interpret_json dt j in
            match (locales,outcomes) with
            | ([],_) -> actions
            | (_,[]) -> actions
            | (locales,outcomes) -> (locales,outcomes) :: actions)
         [] 
         (Yojson.Basic.seq_from_file fn)) in
  (if false
   then
     (List.iter (fun act ->
          print_endline (string_of_action act))
         acts;
      flush_all ()));
  (dt,acts)
