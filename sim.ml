open Utils
module LS = Litset
module F = Aform

(* Turn formula f into a list of literal sets *)
let rec literal_disj f = 
  let rec ld f = match F.dnf f with
    | F.True -> [LS.empty]
    | F.Or (f,f') -> (ld f) @ (ld f')
    | F.Neg (F.Atm s) -> [LS.singleton false s]
    | F.Atm s -> [LS.singleton true s]
    | F.And (f,f') -> begin match ld f, ld f' with 
        | [m],[n] -> [LS.union m n]
        | _ -> failwith "Should be in DNF" end
    | _ -> failwith "Should be in DNF"
  in f |> ld |> List.filter LS.ok |> uniq LS.equal

(* Given set of literals lits, negate any atom a in f
 *  if ~a is in lits
*)
let dualize lits = 
  let rec dual = function 
    | F.True -> F.True
    | F.Atm s -> if LS.pos s lits then F.Neg (F.Atm s) else F.Atm s
    | F.Or (f,f') -> F.Or (dual f, dual f')
    | F.And (f,f') -> F.And (dual f, dual f')
    | F.Neg f -> F.Neg (dual f)
    | F.Impl (f,f') -> F.Impl (dual f, dual f')
  in fun f -> dual f

let pre s = s.[0] = '\''

(* Given set of literals lits, replace pre-atoms (starting with "'")
 * with either true or false *)
let partial_eval lits =
  let rec pe = function 
    | F.True -> F.True
    | F.Atm s -> if pre s then (if LS.pos s lits then F.True else F.Neg F.True) else F.Atm s
    | F.Or (f,f') -> F.Or (pe f, pe f')
    | F.And (f,f') -> F.And (pe f, pe f')
    | F.Neg f -> F.Neg (pe f)
    | F.Impl (f,f') -> F.Impl (pe f, pe f')
  in fun f -> pe f

(* For every atom in m, store it as a positive literal if it is negative in lits,
 * as a negative otherwise *)
let relativize lits m = 
  List.fold_left (fun acts s -> LS.add (LS.neg s lits) s acts) LS.empty m

(* For every literal set ls in lits_l, find all completions of ls by either assuming or negating every element of atoms not already in ls *)
let complete atoms lits_l =
  let inject_atom ls s =
    let folder ls l =
      if LS.mem s l then l::ls else (LS.add true s l)::(LS.add false s l)::ls in
    List.fold_left folder [] ls
  in
  List.fold_left inject_atom lits_l atoms


(* The following (merge, group, ungroup, clean) merges
 * litsets together when possible, so their presentation is more concise *)
let rec merge ms =

  let fold (lits,xs) l' = 
    (*log ("Comparing "^(str_of_lits lits));*)
    (*log ("with...   "^(str_of_lits l'));*)
    if LS.fusable lits l' 
    then (
      (*log "...fusing"; *)
      (LS.fuse lits l',xs) 
    ) else (
      (*log "...not fusing"; *)
      (lits,l'::xs)
    ) in

  let rec mg ms acc =
    match ms with
    | lits::ms -> let (l',ms') = List.fold_left fold (lits,[]) ms in mg ms' (l'::acc)
    | [] -> acc
  in let res = mg ms [] in
  if List.length res = List.length ms
  then res
  else merge res

let group ls =
  let rec grp ls acc = match ls with
    | (lits,acts)::xs ->
      let (y,n) = List.partition (fun (l,a) -> a = acts) xs in
      grp n (((lits::(List.map fst y)),acts)::acc)
    | [] -> acc
  in grp ls []

let ungroup grps = 
  let rec ugrp grps acc = match grps with
    | (ls,acts)::xs -> ugrp xs ((List.map (fun l -> (l,acts)) ls)::acc)
    | [] -> acc
  in List.flatten (ugrp grps [])

let clean ls = 
  let groups = group ls in
  let merged = List.map (fun (l,a) -> (merge l,a)) groups in
  ungroup merged

(* Some printing methods *)
let str_of_pre all lits = 
  let printer s = 
    if LS.pos s lits 
    then (green s) 
    else 
      if LS.neg s lits 
      then (red s) 
      else (String.make (String.length s) ' ')
  in let strs = List.map printer all
  in String.concat " " strs

let show_header atoms = 
  let s = (String.concat " " atoms)^"  #   actions" in
  let u = (String.make (String.length s) '-')
  in log s; log u

let show_res all (lits,m) =
  log @@ (str_of_pre all lits)^"  #  "^(LS.to_str (relativize lits m))

(* Given preconditions pre and constraints constr, give all possible models *)
let models pre constr = 
  let all_atoms = List.sort_uniq (String.compare) (F.collect pre @ F.collect constr) in
  let lits_l_partial = literal_disj pre in
  let lits_l = complete all_atoms lits_l_partial in

  (*logm "Lits" @@ String.concat " ; " @@ List.map LS.to_str lits_l;*)

  let subs = all_atoms::(sublists all_atoms) in
  let treat lits = 
    let constr' = dualize lits (partial_eval lits constr)
    in let sats = subs 
                  |> List.map (fun acts -> (F.sat1 acts constr', (lits,acts))) 
                  |> List.filter fst
                  |> List.map snd
    in sats
  in (all_atoms, List.flatten @@ List.map treat lits_l)


let run pre post = 
  logm "Pre   " (F.str_of_form pre);
  logm "Constr" (F.str_of_form post);
  log "";
  let (all,ms) = models pre post in
  let cleaned = clean ms in
  show_header all;
  List.iter (show_res all) cleaned; 
  log ""

let test () = let open F in
  let f =  ~* !"c" |* ((! "ax") ^* (! "b"))
  in run f (!"c");
  let f = ~* !"a" => !"b" in
  run (!"a" |* ~* !"a") f ;
  let pre = !"d" ^* ~* !"e" ^* ~* !"c" in
  let post = ~* !"e" => !"c" in
  run pre post;
  let pre = True
  in run pre post;
  run (~* !"a" |* ~* !"b") (!"'a" => !"b")

let histfile =  "./session_history"

let rec prompt () =
  match LNoise.linenoise "> " with
  | None -> 
    log "Attempting to save session history";
    ignore (LNoise.history_save histfile)
  | Some v -> 
    (match Parser.parse v with
     | Ok (mform,constr) -> run (match mform with | Some form -> form | _ -> F.True) constr
     | Error _ -> log "Parse error.");
    ignore(LNoise.history_add v);  
    prompt()

let () = 
  ignore (LNoise.history_load histfile);
  if (Array.length Sys.argv) > 1 && Sys.argv.(1) = "test" 
  then test ()
  else prompt ()
