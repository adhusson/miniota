module S = Set.Make(String)
open Utils

type t = S.t * S.t * bool

let add pol s (pos,neg,error) = 
  let s = get s in
  if error 
  then (pos,neg,error) 
  else begin
    if pol 
    then (if S.mem s neg then (pos,neg,true) else (S.add s pos, neg, error))
    else (if S.mem s pos then (pos,neg,true) else (pos, S.add s neg, error))
  end

let pos s (pos,neg,error) = S.mem (get s) pos
let neg s (pos,neg,error) = S.mem (get s) neg
let mem s l = pos s l || neg s l

let empty = (S.empty, S.empty, false)

let singleton pol s = if pol
  then (S.singleton (get s), S.empty, false)
  else (S.empty, S.singleton (get s), false)

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
  and impl p n p' n' = S.subset p' p && S.subset n' n
  in (check p n p' n') || (check p' n' p n) || (impl p n p' n') || (impl p' n' p n)

let fuse (p,n,e) (p',n',e') =
  ((S.inter p p'), (S.inter n n'), e || e')

let to_ls lits = List.map (fun (b,s) -> (if b then "+" else "-")^s) (elements lits)

let to_str lits = String.concat " " (to_ls lits)

