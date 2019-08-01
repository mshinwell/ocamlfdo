(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*                         based on the work of                           *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Control flow graph representation of a function's code. *)

module Label : Identifiable.S with type t := Linearize.label

(* CR gyorsh: store [label_after] separately and update after reordering. *)
type func_call_operation =
  | Indirect of { label_after : Label.t; }
  (* CR mshinwell: Rename [Immediate] -> [Direct]? *)
  | Immediate of {
      func : string;
      label_after : Label.t;
    }

type prim_call_operation =
  | External of {
      func : string;
      alloc : bool;
      label_after : Label.t;
    }
  | Alloc of {
      bytes : int;
      label_after_call_gc : label option;
      spacetime_index : int;
    }
  | Checkbound of {
      immediate : int option;
      label_after_error : label option;
      spacetime_index : int;
    }

type operation =
  | Move
  | Spill
  | Reload
  | Const_int of nativeint
  | Const_float of int64
  | Const_symbol of string
  | Stackoffset of int
  | Load of Cmm.memory_chunk * Arch.addressing_mode
  | Store of Cmm.memory_chunk * Arch.addressing_mode * bool
  | Intop of Mach.integer_operation
  | Intop_imm of Mach.integer_operation * int
  | Negf
  | Absf
  | Addf
  | Subf
  | Mulf
  | Divf
  | Floatofint
  | Intoffloat
  | Specific of Arch.specific_operation
  | Name_for_debugger of {
      ident : Ident.t;
      which_parameter : int option;
      provenance : unit option;
      is_assignment : bool;
    }

type call_operation =
  | P of prim_call_operation
  | F of func_call_operation

type condition =
  | Always
  | Test of Mach.test

type successor = condition * Label.t

type 'a instruction = {
  desc : 'a;
  arg : Reg.t array;
  res : Reg.t array;
  dbg : Debuginfo.t;
  live : Reg.Set.t;
  trap_depth : int;
  (* CR gyorsh: make [id] into an abstract type to distinguish special cases of
     new ids explicitly. *)
  id : int;
}

type basic =
  | Op of operation
  | Call of call_operation
  | Reloadretaddr
  | Entertrap
  | Pushtrap of { lbl_handler : Label.t; }
  | Poptrap
  | Prologue

(* CR gyorsh: [Switch] has successors but currently no way to attach User_data
   to them. Can be fixed by translating Switch to Branch.
   mshinwell: Is this CR still relevant?
*)
module Terminator : sig
  type t =
    | Branch of successor list
    | Switch of Label.t array
    | Return
    | Raise of Cmm.raise_kind
    | Tailcall of func_call_operation
end

module Basic_block : sig
  (** Sequences of code without any control flow constructs (save for
      asynchronous exceptions e.g. from allocation points). *)
  type t = {
    start : Label.t;
    mutable body : basic instruction list;
    mutable terminator : terminator instruction;
    mutable predecessors : Label.Set.t;
  }

  val successors : t -> successor list

  val successor_labels : t -> Label.t list
end

(** Control flow graph of a function. *)
type t = {
 blocks : Basic_block.t Label.Tbl.t;
  (** Map from labels to basic blocks. *)
  fun_name : string;
  (** Function name, used for printing messages. *)
  entry_label : Label.t;
  (** The label of the block that must always be first in any layout of
      this CFG. *)
}

(** Debug printing. *)
val print
   : out_channel
  -> t
  -> Label.t list
  -> basic_to_linear:(
       basic instruction
    -> Linearize.instruction
    -> Linearize.instruction)
  -> linearize_terminator:(
       terminator instruction
    -> Linearize.instruction)
  -> unit

val print_terminator : Format.formatter -> terminator instruction -> unit
