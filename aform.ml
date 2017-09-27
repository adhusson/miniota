open Utils

type 'a aform = 
    Atm of 'a | 
    Neg of 'a aform | 
    Or of 'a aform * 'a aform | 
    And of 'a aform * 'a aform | 
    Impl of 'a aform * 'a aform |
    True

type form = string aform
type bform = (bool * string) aform
type model = string list

let (!) x = Atm x
let (~*) x = Neg x
let (|*) x y = Or (x,y)
let (^*) x y = And (x,y)
let (=>) x y = Impl (x,y)

let rec str_of_aform f t = 
  let n ff = str_of_aform ff t in
  match f with
  | Neg True -> "!"
  | Atm o -> t o
  | Impl (f, f') -> "("^(n f)^" => "^(n f')^")"
  | Neg f -> "~"^(n f)
  | And (f, f') -> "("^(n f)^" ^ "^(n f')^")"
  | Or (f, f') -> "("^(n f)^" v "^(n f')^")"
  | True -> "T"

let rec str_of_form f = str_of_aform f (fun x -> x)

let rec eval m1 m2 form = match form with
  | Atm (b,s) -> if b then List.mem s m2 else List.mem s m1 
  | Neg form' -> not (eval m1 m2 form')
  | Or (form1, form2) -> (eval m1 m2 form1) || (eval m1 m2 form2)
  | And (form1, form2) -> (eval m1 m2 form1) && (eval m1 m2 form2)
  | Impl (form1, form2) -> (not @@ eval m1 m2 form1) || (eval m1 m2 form2)
  | True -> true

let rec to_b b f = match f with
  | Atm s -> ! (b,s)
  | Impl (f, f') -> to_b b f => to_b b f'
  | Neg f -> ~* (to_b b f)
  | And (f, f') -> to_b b f ^* to_b b f'
  | Or (f, f') -> to_b b f |* to_b b f'
  | True -> True

let rec dnf f = 
  let rec flat f = match f with
    | True -> True
    | Atm s -> Atm s
    | Or (f,f') -> Or (flat f, flat f')
    | Impl (f,f') -> flat @@ Or (Neg f, f')
    | And (f,f') -> flat_and (flat f) (flat f')
    | Neg f -> begin match f with
        | True -> Neg True
        | Atm s -> Neg (Atm s)
        | Neg f -> flat f
        | And(f,f') -> flat @@ Or (Neg f, Neg f')
        | Or (f,f') -> flat @@ And (Neg f, Neg f')
        | Impl (f,f') -> flat @@ And (f, (Neg f')) end
  and flat_and f f' = match f, f' with
    | Or (g,g'), h | h, Or (g,g') -> Or (flat_and g h, flat_and g' h)
    | f,f' -> And (f,f') 
  in flat f

let left f = to_b false f
let right f = to_b true f

let collect f =
  let rec c' f = match f with
    | True -> []
    | Atm s -> [get s]
    | Impl (f,f') | Or (f,f') | And (f,f') 
      -> (c' f) @ (c' f')
    | Neg f -> c' f
  in uniq (=) (c' f)

(* Rewrite according to FLP *)
let rec rw1 form = match form with
  | True -> True
  | Atm s -> right (! s)
  | Impl (form1, form2) -> ~* (left form1 ^* right form1) |* rw1 form2
  | Neg form' -> ~* (rw1 form')
  | And (f, f') -> rw1 f ^* rw1 f'
  | Or (f, f') -> rw1 f |* rw1 f'

(* Rewrite according to FLPT *)
let rec rw2 form = match form with
  | True -> True
  | Atm s -> right (! s)
  | Impl (form1, form2) -> 
    (~* (left form1 ^* right form1) |* right form2) ^* (~* (left form1) |* (left form2))
  | Neg form' -> ~* (rw2 form')
  | And (f, f') -> rw2 f ^* rw2 f'
  | Or (f, f') -> rw2 f |* rw2 f'

let sat m f = eval m [] (left f)

(* Sat of modem m on formula f, modulo a 2nd-order formula
 * on the minimal models of f. The requirement is of the form:
 * f(m) && ~ exists m' < m, (rewriter f)(m,m')
*)
let rec snd_order_sat rewriter m f = 
  let f' = rewriter f in
  let has_submodels = List.exists (fun m' -> eval m m' f') (sublists m) in 
  (sat m f) && not has_submodels

let rec sat1 m f = snd_order_sat rw1 m f
