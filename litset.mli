type t
val add : bool -> string -> t -> t
val pos : string -> t -> bool
val neg : string -> t -> bool
val mem : string -> t -> bool
val empty : t
val singleton : bool -> string -> t
val ok : t -> bool
val union : t -> t -> t
val equal : t -> t -> bool
val elements : t-> (bool * string) list
val fusable : t-> t -> bool
val fuse : t -> t -> t
val to_ls : t -> string list
val to_str : t -> string
