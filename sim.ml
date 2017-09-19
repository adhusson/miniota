(* Utils *)
let log s = print_string (s^"\n")
let logb b = log @@ string_of_bool b
let logm m s = log (m^": "^s)
let logmb m b = logm m @@ string_of_bool b
let green s = "\x1b[1;32m"^s^"\x1b[0m"
let red s   = "\x1b[1;31m"^s^"\x1b[0m"

let rec uniq eq l = match l with
  | [] -> []
  | x::xs -> x::(uniq eq (List.filter (fun y -> not (eq x y)) xs))


(* Formulas *)
type 'a r_form = Atm of 'a | Neg of 'a r_form | Or of 'a r_form * 'a r_form | And of 'a r_form * 'a r_form | Impl of 'a r_form * 'a r_form

let rec rf_to_str f t = 
  let n ff = rf_to_str ff t in
  match f with
  | Atm o -> t o
  | Impl (f, f') -> "("^(n f)^" => "^(n f')^")"
  | Neg f -> "~"^(n f)
  | And (f, f') -> "("^(n f)^" ^ "^(n f')^")"
  | Or (f, f') -> "("^(n f)^" v "^(n f')^")"

let rec str_of_f f = rf_to_str f (fun x -> x)

let atm x = Atm x
let (!) x = Atm x
let non x = Neg x
let (~*) x = Neg x
let ou x y = Or (x,y)
let (|*) x y = Or (x,y)
let et x y = And (x,y)
let (^*) x y = And (x,y)
let impl x y = Impl (x,y)
let (=>) x y = Impl (x,y)

let collect f =
  let rec c' f = match f with
    | Atm s -> [s]
    | Impl (f,f') | Or (f,f') | And (f,f') 
      -> (c' f) @ (c' f')
    | Neg f -> c' f
  in uniq (=) (c' f)


type form = string r_form
(* Formulas bound to models *)
type b_form = (bool * string) r_form

type model = string list

let rec eval m1 m2 form = match form with
  | Atm (b,s) -> if b then List.mem s m2 else List.mem s m1 
  | Neg form' -> not (eval m1 m2 form')
  | Or (form1, form2) -> (eval m1 m2 form1) || (eval m1 m2 form2)
  | And (form1, form2) -> (eval m1 m2 form1) && (eval m1 m2 form2)
  | Impl (form1, form2) -> (not @@ eval m1 m2 form1) || (eval m1 m2 form2)

let rec to_b b f = match f with
  | Atm s -> ! (b,s)
  | Impl (f, f') -> to_b b f => to_b b f'
  | Neg f -> ~* (to_b b f)
  | And (f, f') -> to_b b f ^* to_b b f'
  | Or (f, f') -> to_b b f |* to_b b f'

let rec dnf f = 
  let rec flat f = match f with
  | Atm s -> Atm s
  | Or (f,f') -> Or (flat f, flat f')
  | Impl (f,f') -> flat @@ Or (Neg f, f')
  | And (f,f') -> flat_and (flat f) (flat f')
  | Neg f -> begin match f with
    | Atm s -> Neg (Atm s)
    | Neg f -> flat f
    | And(f,f') -> flat @@ Or (Neg f, Neg f')
    | Or (f,f') -> flat @@ And (Neg f, Neg f')
    | Impl (f,f') -> flat @@ And (f, (Neg f')) end
  and flat_and f f' = match f, f' with
    | Or (g,g'), h | h, Or (g,g') -> Or (flat_and g h, flat_and g' h)
    | f,f' -> And (f,f') 
  in flat f

(* Double literal set with error *)
module type DS = sig
  type t
  val add : bool -> string -> t -> t
  val pos : string -> t -> bool
  val neg : string -> t -> bool
  val empty : t
  val singleton : bool -> string -> t
  val ok : t -> bool
  val union : t -> t -> t
  val equal : t -> t -> bool
  val elements : t-> (bool * string) list
  val fusable : t-> t -> bool
  val fuse : t -> t -> t
end


let a = ref false

module LS : DS = struct
  module S = Set.Make(String)
  type t = S.t * S.t * bool
  let add pol s (pos,neg,error) = 
    if error 
    then (pos,neg,error) 
    else begin
      if pol 
      then (if S.mem s neg then (pos,neg,true) else (S.add s pos, neg, error))
      else (if S.mem s pos then (pos,neg,true) else (pos, S.add s neg, error))
    end

  let pos s (pos,neg,error) = S.mem s pos
  let neg s (pos,neg,error) = S.mem s neg

  let empty = (S.empty, S.empty, false)

  let singleton pol s = if pol
    then (S.singleton s, S.empty, false)
    else (S.empty, S.singleton s, false)

  let ok (pos,neg,error) = not error

  let union (p,n,e) (p',n',e') = 
    if e || e' || not (S.is_empty (S.inter p n')) || not (S.is_empty (S.inter n p')) then
      (S.empty, S.empty, true)
    else (S.union p p', S.union n n', false)

  let equal (p,n,e) (p',n',e') = S.equal p p' && S.equal n n' && e = e'

  let elements (p,n,_) = 
    let mb b l = List.map (fun s -> (b,s)) l 
    in let elems = mb true (S.elements p) @ mb false (S.elements n)
    in List.sort (fun (_,s) (_,s') -> String.compare s s') elems

  let fusable (p,n,_) (p',n',_) =
    let check p n p' n' =
      let i = S.inter p n' in
      S.cardinal i <= 1 && (S.equal (S.union p' i) p) && (S.equal (S.union n i) n')
    in (check p n p' n') || (check p' n' p n)

  let fuse (p,n,e) (p',n',e') =
    ((S.inter p p'), (S.inter n n'), e || e')

end 

let rec literal_disj f = 
  let g = dnf f in
  logm "dnf" (str_of_f g);
  let rec ld f = match dnf f with
  | Or (f,f') -> (ld f) @ (ld f')
  | Neg (Atm s) -> [LS.singleton false s]
  | Atm s -> [LS.singleton true s]
  | And (f,f') -> begin match ld f, ld f' with 
    | [m],[n] -> [LS.union m n]
    | _ -> failwith "Should be in DNF" end
  | _ -> failwith "Should be in DNF"
  in f |> ld |> List.filter LS.ok |> uniq LS.equal
    
let left f = to_b false f
let right f = to_b true f

(* Rewrite according to FLP *)
let rec rw1 form = match form with
  | Atm s -> right (! s)
  | Impl (form1, form2) -> ~* (left form1 ^* right form1) |* right form2
  | Neg form' -> ~* (rw1 form')
  | And (f, f') -> rw1 f ^* rw1 f'
  | Or (f, f') -> rw1 f |* rw1 f'

(* Rewrite according to FLPT *)
let rec rw2 form = match form with
  | Atm s -> right (! s)
  | Impl (form1, form2) -> 
    let l = ~* (left form1 ^* right form1) |* right form2
    and r = left form1 => left form2
    in l ^* r 
  | Neg form' -> ~* (rw2 form')
  | And (f, f') -> rw2 f ^* rw2 f'
  | Or (f, f') -> rw2 f |* rw2 f'

let submodels m = 
  let rec submodels' m = match m with
    | [] -> [[]]
    | x::xs -> let s = submodels' xs in
      (List.map (fun l -> x::l) s) @ s
  in match (submodels' m) with
  | x::xs -> xs
  | _ -> failwith "Impossible"

;;

let sat m f = eval m [] (left f)

let str_of_m m = "{"^(String.concat ", " m)^"}"
let str_of_subs subs = 
  String.concat "  --  " (List.map str_of_m subs)

let rec sat_generic g m f = 
  let sat = sat m f
  and ff = g f
  in
  (*logm "sat" @@ string_of_bool sat;*)
  (*logm "subs" @@ String.concat "  --  " (List.map (fun l -> "{"^(String.concat ", " l)^"}") subs);*)
  let has_submodels = List.exists (fun m' -> eval m m' ff) (submodels m)
  in 
  (*logm "has_subs" @@ string_of_bool has_submodels;*)
  sat && not has_submodels


let rec sat1 = sat_generic rw1

let rec sat2 = sat_generic rw2

  (*covering subsets : sets of ternary formula which cover the entire truth table. Starting from the smallest, see if it works. For instance starting from ? ?, find minimal models. on the way, record everything. if I find a discrepancy, find the next minimal subset which separates them. So say I split on a, then b, then c. I get +a,-b,-c l1 / +a,-b,+c l1*)

(*Just find 2 formulas which differ only by +/-/?. Merge. Repeat.*)



let models_generic s satN f =
  logm (s^" for") @@ str_of_f f;
  let maxmodel = collect f in
  let all = maxmodel::(submodels maxmodel) in
  let sats = List.map (fun m -> (m,satN m f)) all in
  List.iter (fun (m,s) -> log ((if s then "⊤" else " ")^"  "^(str_of_m m))) sats;
  log ""



let models1 = models_generic "FLP" sat1
let models2 = models_generic "FLPT" sat2


let ldiff l l' = List.filter (fun x -> not @@ List.mem x l') l

let with_choices atoms = List.map (fun a -> Or (Atm a,Neg (Atm a))) atoms

let dualize lits = 
  let rec d = function 
  | Atm s -> if LS.pos s lits then Neg (Atm s) else Atm s
  | Or (f,f') -> Or (d f, d f')
  | And (f,f') -> And (d f, d f')
  | Neg f -> Neg (d f)
  | Impl (f,f') -> Impl (d f, d f')
  in fun f -> d f

let ls_of_lits lits = List.map (fun (b,s) -> (if b then "+" else "-")^s) (LS.elements lits)

let str_of_lits lits = String.concat " " (ls_of_lits lits)


let models (pre:string r_form) constr = 
  let pre_atoms = collect pre
  and constr_atoms = collect constr in
  let all_atoms = List.sort_uniq (String.compare) (pre_atoms @ constr_atoms) in
  let choices = with_choices all_atoms in
  let pre' = List.fold_left (^*) pre choices in

  logm "With choices" @@ str_of_f pre';

  let lits_l = literal_disj pre' in

  logm "Lits" @@ String.concat " ; " @@ List.map str_of_lits lits_l;

  let subs = all_atoms::(submodels all_atoms) in
  let treat lits = 
       let constr' = dualize lits constr
       in let sats = subs 
                     |> List.map (fun acts -> (sat1 acts constr', (lits,acts))) 
                     |> List.filter fst
                     |> List.map snd
       in sats
  in (all_atoms, List.flatten @@ List.map treat lits_l)

let rec merge ms =

  let fold (lits,xs) l' = 
    log ("Comparing "^(str_of_lits lits));
    log ("with...   "^(str_of_lits l'));
    if LS.fusable lits l' 
    then (log "...fusing"; (LS.fuse lits l',xs)) 
    else (log "...not fusing"; (lits,l'::xs)) in

  let rec mg ms acc =
    match ms with
    | lits::ms ->
      let (l',ms') = List.fold_left fold (lits,[]) ms in
      mg ms' (l'::acc)
    (*if List.length ms = List.length ms' *)
    (*then mg ms (l'::acc)*)
    (*else *)
    (*mg (l'::ms') acc*)

    | [] -> acc
  in 
  let res = mg ms [] in
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




(*let models2 pre constr =*)
  (*let all_atoms = uniq (=) (collect pre @ collect constr) in*)
  (*let lits_l = literal_disj pre' in*)
  (*let process lits =*)
    (*solve lits all_atoms*)
  (*List.iter*)

                
let to_lits lits m = 
  List.fold_left (fun acts s -> LS.add (LS.neg s lits) s acts) LS.empty m

let show_pre all lits = 
  let printer s = 
    (if LS.pos s lits then (green s) else if LS.neg s lits then (red s) else " ")
  in let strs = List.map printer all
  in String.concat " " strs

let show_acts lits = String.concat " " (ls_of_lits lits)

let show_res all (lits,m) =
  log @@ (show_pre all lits)^"  =>  "^(show_acts (to_lits lits m))


let run pre post = 
  logm "Formula" (str_of_f pre);
  logm "Post" (str_of_f post);
  let (all,ms) = models pre post
  in List.iter (show_res all) (clean ms)

let n () = 
  let f =  ~* !"c" |* ((! "a") ^* (! "b"))
  in run f (!"c");
  let f = (impl (non (atm "a")) (atm "b")) in
  run (!"a" |* ~* !"a") f 


let () = ignore begin
  n()
end


let m () = ignore begin
  let f = (ou (atm "a") (et (atm "a") (atm "b")))
  in 
  logm "emptysat" @@ string_of_bool @@ sat [] f;
  log (str_of_f f );
  log @@ string_of_bool @@ sat1 ["a"] f;
  log "----------";
  let fA = (impl (non (atm "a")) (atm "b")) in
  models1 fA;
  models2 fA;
  let gA = (impl (non (atm "e")) (atm "c")) in
  let gB = (impl (non (atm "d")) (atm "e")) in
  let fB = ou gA gB in
  models1 fB;
  models2 fB;
  let fC = ou (non gA) (non gB) in
  models1 fC;
  models2 fC;
  let fD = non @@ impl (non (atm "a")) (non (atm "b")) in
  let test f = (models1 f; models2 f) in
  test fD;
  let fD = non @@ impl ((atm "a")) (non (atm "b")) in
  let test f = (models1 f; models2 f) in
  test fD;
  let gA = (impl ((atm "e")) (non (atm "c"))) in
  let gB = (impl ((atm "d")) (non (atm "e"))) in
  test (ou (non gA) (non gB));
  test (ou gA gB);


end

