(* Control Flow Graph of a function. *)
type t

type label = Linearize.label

module Layout : sig
  type t = label list
end

val from_linear : Linearize.fundecl -> t
val to_linear : t -> Linearize.instruction

(* Mutates t *)
val eliminate_dead_blocks : t -> unit

val get_layout : t -> Layout.t
val set_layout : t -> Layout.t -> t

val get_name : t -> string

val id_to_label : t -> int -> label option

val print : t -> unit