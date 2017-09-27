(* 
Set of literals, represents partial models.

For an atom [a] true in the model [ls], [S.pos a ls] is true.
For an atom [a] false in the model [ls], [S.neg a ls] is true.
If the value of atom [a] is unknown in [ls], [S.mem a ls] is false.

If s contains a positive and a negative literal for the same atom, it goes to an error state.
*)

(* The type of a partial model *)
type t

(* Add atom to model. 
[add true s ls] adds the corresponding positive literal.
[add false s ls] adds the corresponding negative literal.*)
val add : bool -> string -> t -> t

(* Test if atom is true in model *)
val pos : string -> t -> bool

(* Test if atom is false in model *)
val neg : string -> t -> bool

(* Test if atom value is known in model *)
val mem : string -> t -> bool

val empty : t
val singleton : bool -> string -> t

(* Returns true iff the model is in an error state, 
i.e. it contains a positive and negative literal for the same atom. 
Once an operation puts a model in an error state, there are 0 guarantees as to its content. *)
val ok : t -> bool

val union : t -> t -> t
val equal : t -> t -> bool

(* Returns all atoms known in the model as a list.
An element (true,s) indicates that s is true in the model, while (false,s) means that s is false. *)
val elements : t-> (bool * string) list

(* A partial model represents all its possible completions. 
For instance {a=true, c=?} represents [{a=true,c=false};{a=true,c=true}].

[fusable ls1 ls2] returns true if there is a model [ls] whose completion is the sum of the completion of [ls1] and [ls2]. *)
val fusable : t-> t -> bool

(* If [ls1] and [ls2] are fusable, [fuse ls1 ls2] returns their lub in the information order.
Otherwise, the result is not specified. *)
val fuse : t -> t -> t

val to_ls : t -> string list
val to_str : t -> string
