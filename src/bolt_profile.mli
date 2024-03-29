(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(* Format generated by BOLT using perf2bolt tool. *)
(* Used here for debugging and testing *)

module Bolt_loc : sig
  type t

  val get_sym : t -> (string * int) option
end

module Bolt_branch : sig
  type t = {
    src : Bolt_loc.t;
    dst : Bolt_loc.t;
    mis : Int64.t;
    count : Int64.t
  }
end

type t = Bolt_branch.t list [@@deriving sexp]

val create :
  Aggregated_decoded_profile.t ->
  Aggregated_perf_profile.t ->
  Elf_locations.t ->
  t

(* Writes directly to file without creating the entire profile in memory. *)
val save :
  Aggregated_decoded_profile.t ->
  Aggregated_perf_profile.t ->
  Elf_locations.t ->
  filename:string ->
  unit

(* Writes directly to file without creating the entire profile in memory. *)
val save_fallthrough :
  Aggregated_decoded_profile.t -> Elf_locations.t -> filename:string -> unit

val read : filename:string -> t

val write : t -> filename:string -> unit
