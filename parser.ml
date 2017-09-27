open Angstrom
open Utils
module F = Aform

let ws = skip_while (function ' ' -> true | _ -> false)
let par p = ws *> char '(' *> ws *> p <* ws <* char ')'<* ws

let atm_char = function 
| 'v' | 'T' -> false 
| 'a'..'z'| 'A'..'Z' | '0'..'9' -> true 
| _ -> false
let atm extended = 
  ws *> 
  (satisfy (fun c -> atm_char c || extended && c = '\'')  >>= fun c ->
  take_while atm_char >>| fun s -> 
  (F.Atm ((Scanf.unescaped (Char.escaped c))^s)) )
  <* ws
let tr = ws *> char 'T' *> return F.True <* ws
let fl = ws *> char '!' *> return (F.Neg F.True) <* ws
let im = string "=>" *> return (fun x y -> F.Impl(x,y))
let ou = char 'v' *> return (fun x y -> F.Or(x,y))
let et = char '^' *> return (fun x y -> F.And(x,y))
let no = ws *> char '~' *> return (fun x -> F.Neg x)
let sim = string "->" *> return (fun x y -> F.Or (F.Neg x,y))

(* generic infix parser by inhabitedtype *)
let infix e op =
  let rec go acc =
    (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
  e >>= fun init -> go init

let recur extended p =
  let base = (par p) <|> atm extended <|> tr <|> fl in
  let neg = (no <*> base) <|> base in
  let conj = infix neg et in
  let impl = infix conj im in
  let simpl = infix impl sim in
  infix simpl ou

let formula_p = fix (recur false)
let constr_p = fix (recur true)

let pair = formula_p >>= fun form -> char '#' *> constr_p >>| fun constr -> (Some form,constr)

let expr = (pair <|> (constr_p >>| fun constr -> (None,constr))) <* end_of_input

let parse s = parse_only expr (`String s)

let parse_test s = 
let res = match parse s with
| Result.Ok (mform,constr) -> (match mform with 
                        | None -> "1 "^(F.str_of_form constr)
                        | Some form -> "2 "^(F.str_of_form form)^" # "^(F.str_of_form constr)
)
| Result.Error s -> "!! "^s
in log (s^" => "^res)

let test () =  begin
parse_test "a ^ b";
parse_test "'a ^ b";
parse_test "'a ^ b # d"; (*no*)
parse_test "a ^ b v c # 'a ^ 'bc";
parse_test "a v b ^ c";
parse_test "~a ^ b";
parse_test "~(a ^ b)";
parse_test "a v b";
parse_test "a v ~b";
parse_test "a v (cj ^ ~b)";
parse_test "(~(a^v) v c) v ~(d) ^ ~c ^ b"
end

(*let () = test()*)

