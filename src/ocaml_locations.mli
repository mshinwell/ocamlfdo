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
(* This is a little wrapper around Elf_locations to manage the file names
   specific to ocaml source and compiler's IR. *)

type t =
  | Source
  | Linear

val decode_line :
  Elf_locations.t ->
  program_counter:Addr.t ->
  string ->
  t ->
  (string * int) option

val to_address : Elf_locations.t -> string -> int -> t -> Addr.t option

(* is filename of type t, i.e., does it have an expected extension. *)
val is_filename : t -> string -> bool

(* append the expected extension *)
val make_filename : t -> string -> string
