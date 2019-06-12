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
open Core
open Block_info

(* Map basic blocks of this function to breakdown of execution counts *)
(* blocks = Block_info.t Hashtbl.M(Cfg_label).t *)

let mal t count =
  if !verbose then printf "Malformed trace with %Ld counts.\n" count;
  t.malformed_traces <- Int64.(t.malformed_traces + count)

let get_loc t addr = Hashtbl.find_exn t.addr2loc addr

let get_block_info t label =
  Hashtbl.find_or_add t.blocks label ~default:(fun () ->
      Block_info.mk ~label )

let record t ~label ~count =
  let b = get_block_info t label in
  Block_info.add b ~count

let get_linearid (loc : Loc.t) =
  let dbg = Option.value_exn loc.dbg in
  dbg.line

(* Find basic instruction whose id=[linearid] in [block] *)
let get_basic_instr linearid (block : Cfg.block) =
  List.find block.body ~f:(fun instr -> instr.id = linearid)

(* Find basic instruction right before the one with [linearid] in [block].
   If not found linearid or linearid is the first instruction, then return
   None. *)
exception Found_prev of Cfg.basic Cfg.instruction option

let prev_instr linearid block =
  let open Cfg in
  try
    ignore
      ( List.fold block.body ~init:None ~f:(fun prev instr ->
            if instr.id = linearid then raise (Found_prev prev)
            else Some instr )
        : Cfg.basic Cfg.instruction option );
    None
  with Found_prev prev -> prev

(* Find the block in [cfg] that contains [loc] using its linearid *)
let get_block (loc : Loc.t) cfg =
  match loc.dbg with
  | None ->
      ( if !verbose then
        let rel = Option.value_exn loc.rel in
        printf "No linearid for 0x%Lx in func %d at offsets %d\n" loc.addr
          rel.id rel.offset );
      None
  | Some dbg -> (
    match Cfg_builder.id_to_label cfg dbg.line with
    | None ->
        failwithf "No cfg label for linearid %d in %s" dbg.line dbg.file ()
    | Some label -> (
      match Cfg_builder.get_block cfg label with
      | Some block -> Some block
      | None ->
          failwithf
            "Can't find cfg basic block labeled %d for linearid %d in %s\n"
            label dbg.line dbg.file () ) )

let record_intra
    (from_loc : Loc.t) (to_loc : Loc.t) _t count mispredicts func cfg =
  let from_block = get_block from_loc cfg in
  let to_block = get_block to_loc cfg in
  match (from_block, to_block) with
  | None, None ->
      if !verbose then
        printf
          "Ignore intra branch count %Ld from 0x%Lx to 0x%Lx, can't map to \
           CFG\n"
          count from_loc.addr to_loc.addr
  | Some from_block, None ->
      if !verbose then
        printf
          "Ignore intra branch count %Ld from 0x%Lx to 0x%Lx, can't map \
           target to CFG\n"
          count from_loc.addr to_loc.addr;
      Func.record func ~label:from_block.start ~count
  | None, Some to_block ->
      if !verbose then
        printf
          "Ignore intra branch count %Ld from 0x%Lx to 0x%Lx, can't map \
           source to CFG\n"
          count from_loc.addr to_loc.addr;
      Func.record func ~label:to_block.start ~count
  | Some from_block, Some to_block -> (
      Func.record func ~label:from_block.start ~count;
      Func.record func ~label:to_block.start ~count;
      let from_linearid = get_linearid from_loc in
      let to_linearid = get_linearid to_loc in
      let first_instr_id =
        match to_block.body with
        | [] -> to_block.terminator.id
        | hd :: _ -> hd.id
      in
      assert (first_instr_id = to_linearid);
      let bi = Func.get_block_info func from_block.start in
      let b =
        { Block_info.target = to_loc;
          target_label = Some to_block.start;
          intra = true;
          taken = count;
          mispredicts
        }
      in
      if from_block.terminator.id = from_linearid then (
        (* Find the corresponding successor *)
        match from_block.terminator.desc with
        | Return
        (* return from a recursive call *)
        (* target must be right after a call *)
        | Tailcall _ ->
            (* tailcall *)
            failwith "Check not implemented"
        | Raise _ ->
            (* target must be a hanlder block *)
            assert (Cfg_builder.is_trap_handler cfg to_block.start)
        | Branch _ | Switch _ ->
            let successors = Cfg.successor_labels from_block in
            assert (List.mem successors to_block.start ~equal:Int.equal);
            Block_info.add_branch bi b )
      else
        (* recursive call, find the call instruction *)
        let instr =
          Option.value_exn (get_basic_instr from_linearid from_block)
        in
        match instr.desc with
        | Call _ -> Block_info.add_call bi from_loc b
        | _ -> assert false )

let record_exit
    (from_loc : Loc.t) (to_loc : Loc.t) _ count mispredicts func cfg =
  (* Branch going outside of this function. *)
  match get_block from_loc cfg with
  | None ->
      if !verbose then
        printf
          "Ignore inter branch count %Ld from 0x%Lx. Can't map to CFG.\n"
          count from_loc.addr
  | Some from_block -> (
      Func.record func ~label:from_block.start ~count;
      (* Find the corresponding instruction and update its counters. The
         instruction is either a terminator or a call.*)
      let linearid = get_linearid from_loc in
      let terminator = from_block.terminator in
      let bi = Func.get_block_info func from_block.start in
      if terminator.id = linearid then
        (* terminator *)
        match terminator.desc with
        | Branch _ | Switch _ | Tailcall _ ->
            (* can't branch outside the current function *)
            assert false
        | Return | Raise _ ->
            let b =
              { Block_info.target = Some to_loc;
                target_label = None;
                intra = false;
                taken = count;
                mispredicts
              }
            in
            Block_info.add_branch bi b
      else
        (* Call *)
        match get_basic_instr linearid from_block with
        | None -> assert false
        (* we've checked before for presence of dbg info *)
        | Some instr -> (
          match instr.desc with
          | Call _ ->
              let b =
                { Block_info.target = Some to_loc;
                  target_label = None;
                  intra = false;
                  taken = count;
                  mispredicts
                }
              in
              Block_info.add_call bi from_loc b
          | _ -> assert false ) )

let record_entry
    (_from_loc : Loc.t) (to_loc : Loc.t) _t count _mispredicts func cfg =
  (* Branch into this function from another function, which may be unknown.
     One of the following situations: Callee: branch target is the first
     instr in the entry block. Return from call: branch target is a label
     after the call site. Exception handler: branch target is a trap
     handler. *)
  match get_block to_loc cfg with
  | None ->
      if !verbose then
        printf "Ignore inter branch count %Ld to 0x%Lx. Can't map to CFG.\n"
          count to_loc.addr
  | Some to_block -> (
      Func.record func ~label:to_block.start ~count;
      (* Find the corresponding instruction and update its counters.*)
      let linearid = get_linearid to_loc in
      let _bi = Func.get_block_info func to_block.start in
      let first_instr_linearid =
        match to_block.body with
        | [] -> to_block.terminator.id
        | hd :: _ -> hd.id
      in
      (* We could infer call targets from this info, but its not implemented
         yet because we don't have much use for it and it would change the
         way Block_info.add_call maps callsites using locations because we
         don't necessarily have a location for the caller. Also note that
         add_call currently checks that each callee can be installed at most
         once, as guaranteed by the aggregated profile. *)
      if
        first_instr_linearid = linearid
        && ( to_block.start = Cfg_builder.entry_label cfg
           (* Callee *)
           || Cfg_builder.is_trap_handler cfg to_block.start
              (* Exception handler *) )
      then ( (* Block_info of potential callers can be updated *) )
      else
        (* Return from a call. Find predecessor instruction and check that
           it is a call. There could be more than one predecessor. We have
           enough information to find which one in some cases. *)
        let record_call (instr : Cfg.basic Cfg.instruction) =
          match instr.desc with
          | Call _ -> ()
          | _ -> assert false
        in
        let find_prev_block_call () =
          (* find a predecessor block that must end in a call and
             fallthrough. *)
          Cfg.LabelSet.iter
            (fun pred_label ->
              let pred =
                Option.value_exn (Cfg_builder.get_block cfg pred_label)
              in
              match pred.terminator.desc with
              | Branch [ (Always, label) ] when label = to_block.start ->
                  let last_instr = List.last_exn to_block.body in
                  record_call last_instr
              | _ -> assert false )
            to_block.predecessors
        in
        if first_instr_linearid = linearid then find_prev_block_call ()
        else
          match prev_instr linearid to_block with
          | Some instr -> record_call instr
          | None -> (
              (* instr with linearid must be the terminator *)
              assert (to_block.terminator.id = linearid);
              match to_block.body with
              | [] ->
                  (* empty body means the call was in one of the pred blocks *)
                  find_prev_block_call ()
              | _ ->
                  let last_instr = List.last_exn to_block.body in
                  record_call last_instr ) )

(* Depending on the settings of perf record and the corresponding CPU
   configuration, LBR may capture different kinds of branches, including
   function calls and returns. *)
let record_branch t from_loc to_loc count mispredicts (func : Func.t) cfg =
  let open Loc in
  (* at least one of the locations is known to be in this function *)
  match (from_loc.rel, to_loc.rel) with
  | Some from_rel, Some to_rel
    when from_rel.id = func.id && to_rel.id = func.id ->
      record_intra from_loc to_loc t count mispredicts func cfg
  | Some from_rel, _ when from_rel.id = func.id ->
      record_exit from_loc to_loc t count mispredicts func cfg
  | _, Some to_rel when to_rel.id = func.id ->
      record_entry from_loc to_loc t count mispredicts func cfg
  | _ -> assert false

exception Malformed_fallthrough_trace

let compute_fallthrough_execounts t from_lbl to_lbl func cfg =
  (* Get the part of the layout starting from from_block up to but not
     including to_block *)
  try
    let fallthrough =
      Cfg_builder.get_layout cfg
      |> List.drop_while layout ~f:(fun lbl -> not (lbl = from_lbl))
    in
    let fallthrough =
      match List.findi fallthrough ~f (fun lbl -> lbl = to_lbl) with
      | None -> raise Malformed_fallthrough_trace
      | Some (to_pos, _) -> List.take fallthrough to_pos
    in
    (* Check that all terminators fall through *)
    let check_fallthrough src_lbl dst_lbl =
      let block = Option.value_exn (Cfg_builder.get_block cfg src_lbl) in
      match block.terminator.desc with
      | Branch _ | Switch _ ->
          assert (
            List.mem (Cfg.successor_labels block) dst_lbl ~equal:Int.equal
          )
      | _ -> raise Malformed_fallthrough_trace
    in
    List.fold_right fallthrough ~init:to_lbl ~f:check_fallthrough |> ignore;
    (* Account only for the intermediate blocks in the trace. Endpoint
       blocks of the trace are accounted for when we handled their LBR
       branches. *)
    let record_fallthrough src_lbl dst_lbl =
      Func.record func ~label:dst_lbl ~count;
      let bi = Func.get_block_info func src_lbl in
      let b =
        { Block_info.target = None;
          target_label = Some dst_lbl;
          intra = true;
          fallthrough = true;
          taken = count;
          mispredicts = 0L
        }
      in
      Block_info.add_branch bi b
    in
    List.fold_right fallthrough ~init:to_lbl ~f:record_fallthrough |> ignore;
    if !verbose then
      printf "recorded healthy trace from 0x%Lx to 0x%Lx count %Ld\n"
        from_loc.addr to_loc.addr count
  with Malformed_fallthrough_trace ->
    (* If the trace is malformed, don't add counts *)
    mal t count

let record_trace t from_loc to_loc count (func : Func.t) cfg =
  (* both locations must be in this function *)
  let open Loc in
  match (from_loc.rel, to_loc.rel) with
  | Some from_rel, Some to_rel
    when from_rel.id = func.id && to_rel.id = func.id -> (
    match (get_block from_loc cfg, get_block to_loc cfg) with
    | Some from_block, Some to_block when from_block.start = to_block.start
      ->
        if !verbose then
          if from_block.start = to_block.start then
            printf
              "No fallthroughs in trace with count %Ld from 0x%Lx to \
               0x%Lx:from_block = to_block\n"
              count from_loc.addr to_loc.addr
    | Some from_block, Some to_block ->
        compute_fallthrough_execounts t from_block.start to_block.start func
          cfg
    | _ ->
        if !verbose then
          printf
            "Ignoring trace with count %Ld from 0x%Lx to 0x%Lx:cannot map \
             to_loc or from_loc to cfg blocks.\n"
            count from_loc.addr to_loc.addr;
        mal t count )
  | _ ->
      if !verbose then
        printf
          "Ignoring trace with count %Ld from 0x%Lx to 0x%Lx:to and from \
           function is not the same or not known.\n"
          count from_loc.addr to_loc.addr;
      mal t count

(* Translate linear ids of this function's locations to cfg labels within
   this function, find the corresponding basic blocks and update their
   block_info. Perform lots of sanity checks to make sure the location of
   the execounts match the instructions in the cfg. *)
let compute_execounts t func cfg =
  let blocks = Hashtbl.create (module Cfg_label) in
  (* Associate instruction counts with basic blocks *)
  Hashtbl.iteri func.agg.instructions ~f:(fun ~key ~data ->
      let loc = get_loc t key in
      match get_block loc cfg with
      | None ->
          if !verbose then
            printf "Ignore exec count at 0x%Lx\n, can't map to cfg\n"
              loc.addr
      | Some block -> Func.record func ~label:block.start ~count:data );
  (* Associate fall-through trace counts with basic blocks *)
  Hashtbl.iteri func.agg.traces ~f:(fun ~key ~data ->
      let from_addr, to_addr = key in
      let from_loc = get_loc t from_addr in
      let to_loc = get_loc t to_addr in
      record_trace t from_loc to_loc data func cfg );
  ( if !verbose then
    let total_traces =
      List.fold (Hashtbl.data func.agg.traces) ~init:0L ~f:Int64.( + )
    in
    let ratio = Int64.(t.malformed_traces * 100L / total_traces) in
    printf "Found %Ld malformed traces out of %Ld (%Ld)\n"
      t.malformed_traces total_traces ratio );
  (* Associate branch counts with basic blocks *)
  Hashtbl.iteri func.agg.branches ~f:(fun ~key ~data ->
      let mispredicts = Hashtbl.find_exn func.agg.mispredicts key in
      let from_addr, to_addr = key in
      let from_loc = get_loc t from_addr in
      let to_loc = get_loc t to_addr in
      record_branch t from_loc to_loc data mispredicts func cfg );
  blocks

(* CR gyorsh: propagate counts to compute missing fallthroughs? *)

(* Compute detailed execution counts for function [name] using its CFG *)
let compute_cfg_execounts t name cfg =
  match Hashtbl.find t.name2id name with
  | None ->
      if !verbose then printf "Not found profile for %s with cfg.\n" name;
      None
  | Some id ->
      let func = Hashtbl.find_exn t.functions id in
      if func.count > 0L && func.has_linearids then (
        if !verbose then printf "compute_cfg_execounts for %s\n" name;
        Some (compute_execounts t func cfg) )
      else None
