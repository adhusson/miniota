(* Utils *)
let log s = print_string (s^"\n")
let logb b = log @@ string_of_bool b
let logm m s = log (m^": "^s)
let logmb m b = logm m @@ string_of_bool b

let rec uniq l = match l with
  | [] -> []
  | x::xs -> x::(uniq (List.filter (fun y -> x <> y) xs))


(* Formulas *)
type 'a r_form = Atm of 'a | Neg of 'a r_form | Or of 'a r_form * 'a r_form | And of 'a r_form * 'a r_form | Impl of 'a r_form * 'a r_form

let atm x = Atm x
let non x = Neg x
let ou x y = Or (x,y)
let et x y = And (x,y)
let impl x y = Impl (x,y)

let collect f =
  let rec c' f = match f with
    | Atm s -> [s]
    | Impl (f,f') | Or (f,f') | And (f,f') 
      -> (c' f) @ (c' f')
    | Neg f -> c' f
  in uniq (c' f)


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
  | Atm s -> Atm (b,s)
  | Impl (f, f') -> impl (to_b b f) (to_b b f')
  | Neg f -> non (to_b b f)
  | And (f, f') -> et (to_b b f) (to_b b f')
  | Or (f, f') -> ou (to_b b f) (to_b b f')

let left f = to_b false f
let right f = to_b true f

(* Rewrite according to FLP *)
let rec rw1 form = match form with
  | Atm s -> right (atm s)
  | Impl (form1, form2) -> ou (non (et (left form1) (right form1))) (right form2)
  | Neg form' -> non (rw1 form')
  | And (f, f') -> et (rw1 f) (rw1 f')
  | Or (f, f') -> ou (rw1 f) (rw1 f')

let rec rw2 form = match form with
  | Atm s -> right (atm s)
  | Impl (form1, form2) -> 
    let l = (ou (non (et (left form1) (right form1))) (right form2))
    and r = impl (left (form1)) (left (form2))
    in et l r 
  | Neg form' -> non (rw2 form')
  | And (f, f') -> et (rw2 f) (rw2 f')
  | Or (f, f') -> ou (rw2 f) (rw2 f')

let rec rf_to_str f t = 
  let n ff = rf_to_str ff t in
  match f with
  | Atm o -> t o
  | Impl (f, f') -> "("^(n f)^" => "^(n f')^")"
  | Neg f -> "~"^(n f)
  | And (f, f') -> "("^(n f)^" ^ "^(n f')^")"
  | Or (f, f') -> "("^(n f)^" v "^(n f')^")"

let rec f_to_str f = rf_to_str f (fun x -> x)

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


let models_generic s satN f =
  logm (s^" for") @@ f_to_str f;
  let maxmodel = collect f in
  let all = maxmodel::(submodels maxmodel) in
  let sats = List.map (fun m -> (m,satN m f)) all in
  List.iter (fun (m,s) -> log ((if s then "⊤" else " ")^"  "^(str_of_m m))) sats;
  log ""

let models1 = models_generic "FLP" sat1
let models2 = models_generic "FLPT" sat2

let () = ignore begin
  let f = (ou (atm "a") (et (atm "a") (atm "b")))
  in 
  logm "emptysat" @@ string_of_bool @@ sat [] f;
  log (f_to_str f );
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

