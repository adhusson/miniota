let rec uniq eq l = match l with
  | [] -> []
  | x::xs -> x::(uniq eq (List.filter (fun y -> not (eq x y)) xs))

(* Give all strict sublists of list m *)
let sublists m = 
  let rec submodels' m = match m with
    | [] -> [[]]
    | x::xs -> let s = submodels' xs in
      (List.map (fun l -> x::l) s) @ s
  in match (submodels' m) with
  | x::xs -> xs
  | _ -> failwith "Impossible"

let log s = print_endline s
let logb b = log @@ string_of_bool b
let logm m s = log (m^": "^s)
let logmb m b = logm m @@ string_of_bool b
let green s = "\x1b[1;32m"^s^"\x1b[0m"
let red s   = "\x1b[1;31m"^s^"\x1b[0m"

let get s = if s.[0] = '\'' then String.sub s 1 (String.length s - 1) else s

