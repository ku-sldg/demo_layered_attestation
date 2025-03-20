(* Copyright (c) 2025, The MITRE Corporation.
*)


type update_kind =
  | Failing
  | Changed
  | Unchanged

let update_lub = function
  | Failing -> (fun _ -> Failing)
  | Changed -> (function
      | Failing -> Failing
      | _ -> Changed)
  | Unchanged -> (fun v -> v)

let fold_updates f =
  let rec g so_far = function
    | [] -> so_far
    | (a :: rest) -> 
      match (f a) with
      | Failing -> Failing
      | r -> g (update_lub r so_far) rest in
  g Unchanged

let fold_changes f =
  let rec g = function
    | (Failing, _) -> (fun _ -> Failing, [])
    | (sf, es') -> (function
        | [] -> sf, List.rev es'
        | (e :: rest) -> 
          match (f e) with
          | (Failing,_) -> Failing, []
          | (r,e') -> g ((update_lub r sf), e' :: es') rest) in
  g (Unchanged, [])   
  

(* type 'a updater = 'a -> 'a
   type 'a detector = 'a -> update_kind


let check_update_addresses
    u_detector ds_detector text_detector =  
  fold_updates
    (function (Afield (_,u,ds,ts)) ->
       update_lub
         (u_detector u)
         (update_lub
            (ds_detector ds)
            (fold_updates text_detector ts)))  

let update_addresses
    uname_updater domains_updater text_updater
    i afields =
  let new_afields =
    List.map
      (function (Afield (i,u,ds,ts)) ->
         Afield (i,
                 uname_updater u, 
                 domains_updater ds,
                 text_updater ts))
      afields in
  Addresses
    (i,
     format_subfields new_afields,
     new_afields)

*) 

type subst =
  | Strings of (string * string)
  | Lists of (string list * string list) (* update domain list *)

type fieldname = Username | Domainnames | Textfields 

let string_of_fieldname = function
  | Username -> "user"
  | Domainnames -> "domain"
  | Textfields -> "text"

type locale =
  | Header of string
  | Field of (string * fieldname)

let entry_relevant =
  function
  | Header str -> (=) str
  | Field (str,_) -> (=) str

type outcome =
  | Require                     (* If locale absent, fail *)
  | Prohibit                    (* If locale present, discard it *)
  | Substitute of subst         (* replace src -> tgt *)
  | Deletion of string          (* str present means discard locale *)  
  | Insertion of string         (* str absent means insert it *)  
      

type action =
  locale list
  * outcome list                (* meaning, Cartesian product of all
                                   these locales with all these
                                   outcomes *)
    
let did_we_succeed permit_change = function
  | Failing -> (print_endline "failed"); false
  | Changed -> permit_change
  | Unchanged -> true
    
let string_of_locale = function
  | Header str -> str
  | Field (str,f) -> str ^ " : " ^ (string_of_fieldname f)

let string_of_domains =
  String.concat ", " 

let string_of_outcome = function
  | Require -> "Req"
  | Prohibit -> "Pbt"
  | Substitute (Strings (f,t)) -> "[ " ^ f ^ " -> " ^ t ^ " ]"
  | Substitute (Lists (fds, tds)) ->
    "[ " ^ (string_of_domains fds) ^ " -> " ^
    (string_of_domains tds) ^ " ]"
  | Deletion str -> "Del: " ^ str
  | Insertion str -> "Ins: " ^ str

let string_of_action (ls,os) =
  ("{ " ^
   (String.concat "\n  " (List.map string_of_locale ls)) ^
   " }\n< " ^
   (String.concat "
  " (List.map string_of_outcome os)) ^
   " >")

