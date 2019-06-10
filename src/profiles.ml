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

let verbose = ref true

module Addr = struct
  type t = int64 [@@deriving compare,sexp,hash]
end
module Execount = struct
  type t = int64 [@@deriving sexp]
end

module Loc = struct
  (* Dwarf info associated with a location *)
  type dbg = {
    file : string;  (* filename *)
    line : int;     (* line number *)
  } [@@deriving compare, sexp, hash]

  type rel = {
    id : int;       (* Unique id of the containing function symbol *)
    offset : int;   (* Offset from the start of the function *)
  } [@@deriving compare, sexp, hash]

  type t = {
    addr : Addr.t;     (* Raw address in the original binary *)
    rel : rel option; (* Containing function info and relative offset *)
    dbg : dbg option;
  } [@@deriving compare, sexp, hash]

  let to_bolt_string t =
    match t.rel with
    | None -> "0 [unknown] 0"
    | Some rel -> sprintf "1 %d %x" rel.id rel.offset

end

module Aggregated_perf = struct

  module P = struct
    (* Pair of addresses *)
    type t = Addr.t * Addr.t [@@deriving compare, hash, sexp]
  end

  type t = {
    instructions : Execount.t Hashtbl.M(Addr).t;
    branches : Execount.t Hashtbl.M(P).t;
    (* execution count: number of times the branch was taken. *)
    mispredicts : Execount.t Hashtbl.M(P).t;
    (* number of times the branch was mispredicted:
       branch target mispredicted or
       branch direction was mispredicted. *)
    traces : Execount.t Hashtbl.M(P).t;
    (* execution count: number of times the trace was taken. *)
  } [@@deriving sexp]

  let empty =
    { instructions = Hashtbl.create (module Addr);
      branches = Hashtbl.create (module P);
      mispredicts = Hashtbl.create (module P);
      traces = Hashtbl.create (module P);
    }

  let read filename =
    if !verbose then
      printf "Reading aggregated perf profile from %s\n" filename;
    let t =
      match Parsexp_io.load (module Parsexp.Single) ~filename with
      | Ok t_sexp -> t_of_sexp t_sexp
      | Error error ->
        Parsexp.Parse_error.report Caml.Format.std_formatter error ~filename;
        failwith "Cannot parse aggregated profile file"
    in
    if !verbose then begin
      Printf.printf !"Aggregated perf profile:\n%{sexp:t}\n" t;
    end;
    t

  let write t filename =
    if !verbose then
      printf "Writing aggregated perf profile to %s\n" filename;
    let chan = Out_channel.create filename in
    Printf.fprintf chan !"%{sexp:t}\n" t;
    Out_channel.close chan
end

(* It should be Cfg.label, but we can't add sexp to Cfg.
   because the intent is to eventually integrate Cfg in the compiler, which
   doesn't currently use sexp. We use sexp to convert to/from file. *)
module Cfg_label = struct
  type t = int [@@deriving compare,sexp,hash]
end

module Block_info = struct

  (* Successor info  *)
  type b = {
    target : Loc.t;
    target_label : Cfg_label.t option;
    (* cfg label that the target location translates to *)
    intra : bool; (* is the target intraprocedural? *)
    taken : Execount.t;
    mispredicts : Execount.t;
  } [@@deriving sexp]

  (* call site info *)
  type c = {
    callsite : Loc.t;
    mutable callees : b list;
  } [@@deriving sexp]

  (* Execution counts for a basic block *)
  type t = {
    label : Cfg_label.t;
    mutable count : Execount.t; (* Number of times this block was executed. *)
    mutable branches : b list; (* Info about branch targets  *)
    mutable calls : c list (* Info about call targets *)
  } [@@deriving sexp]

  let mk ~label =
    { label;
      count = 0L;
      branches = [];
      calls = [];
    }

  let add t ~count = t.count <- Int64.(t.count + count)

  let add_call t callsite callee =
    (* Find the callsite's info *)
    match List.find t.calls ~f:(fun c -> c.callsite = callsite) with
    | None ->
      let c = { callsite; callees=[callee] } in
      t.calls <- c::t.calls;
    | Some c ->
      (* Check unique call target. *)
      assert (Option.is_none
                (List.find c.callees
                   ~f:(fun b -> b.target = callee.target)));
      c.callees <- callee::c.callees

  let add_branch t b =
    (* Check unique branches target. *)
    assert (Option.is_none (List.find t.branches
                              ~f:(fun b1 -> b1.target = b.target)));
    t.branches <- b::t.branches
end

module Func = struct
  type t = {
    id : int;       (* Unique identifier we assign to this function *)
    name : string;  (* Name of the function symbol *)
    start : Addr.t; (* Raw start address of the function in original binary *)
    mutable count : Execount.t;  (* Preliminary execution count *)
    blocks : Block_info.t Hashtbl.M(Cfg_label).t;
    (* Map basic blocks of this function to breakdown of execution counts *)
    mutable has_linearids : bool; (* Does the function have any linearids? *)
    agg : Aggregated_perf.t;
    (* Counters that refer to this function, uses raw addresses.
       This can be dropped after cfg_count is constructed, to save memory. *)
  } [@@deriving sexp]

  let mk ~id ~name ~start =
    {
      id; name; start;
      has_linearids=false;
      count=0L;
      blocks = Hashtbl.create (module Cfg_label);
      agg = Aggregated_perf.empty;
    }
  (* descending order of execution counts (reverse order of compare) *)
  (* Tie breaker using name.
     Slower than id but more stable w.r.t. changes in perf data and ocamlfdo,
     because ids are an artifact of the way ocamlfdo reads
     and decodes locations.
     Change to tie breaker using id if speed becomes a problem. *)
  let compare f1 f2 =
    let res = compare f2.count f1.count in
    if res = 0 then compare f1.name f2.name
    else res

  let get_block_info t label =
    Hashtbl.find_or_add t.blocks label
      ~default:(fun () -> Block_info.mk ~label)

  let record t ~label ~count =
    let b = get_block_info t label in
    Block_info.add b ~count
end

module Aggregated_decoded = struct
  open Func
  type t = {
    addr2loc : Loc.t Hashtbl.M(Addr).t; (* map raw addresses to locations *)
    name2id : int Hashtbl.M(String).t; (* map func name to func id *)
    functions : Func.t Hashtbl.M(Int).t; (* map func id to func info *)
  }  [@@deriving sexp]

  let mk size =
    { addr2loc = Hashtbl.create ~size (module Addr);
      name2id = Hashtbl.create (module String);
      functions = Hashtbl.create (module Int);
    }

  let get_loc t addr =
    Hashtbl.find_exn t.addr2loc addr

  let get_func t addr =
    let loc = get_loc t addr in
    match loc.rel with
    | None -> None
    | Some rel ->
      let id = rel.id in
      let func = Hashtbl.find_exn t.functions id in
      Some func

  let get_linearid t (loc:Loc.t) =
    let dbg = Option.value_exn loc.dbg in
    dbg.line

  (* Find basic instruction whose id=[linearid] in [block] *)
  let get_basic_instr linearid (block:Cfg.block) =
    List.find block.body ~f:(fun instr -> instr.id = linearid)

  (* Find basic instruction right before the one with [linearid] in [block].
     If not found linearid or linearid is the first
     instruction, then return None. *)
  exception Found_prev of Cfg.basic Cfg.instruction option
  let prev_instr linearid block =
    let open Cfg in
    try
      ignore (List.fold block.body
                ~init:None
                ~f:(fun prev instr ->
                  if instr.id = linearid then
                    raise (Found_prev prev)
                  else
                    Some instr
                ):Cfg.basic Cfg.instruction option);
      None
    with Found_prev prev ->
      prev

  (* Find the block in [cfg] that contains [loc] using its linearid *)
  let get_block (loc:Loc.t) cfg =
    match loc.dbg with
    | None ->
      if !verbose then begin
        let rel = Option.value_exn loc.rel in
        printf "No linearid for 0x%Lx in func %d at offsets %d\n"
          loc.addr rel.id rel.offset
      end;
      None
    | Some dbg ->
      match Cfg_builder.id_to_label cfg dbg.line with
      | None ->
        failwithf "No cfg label for linearid %d in %s" dbg.line dbg.file ()
      | Some label ->
        match Cfg_builder.get_block cfg label with
        | Some block -> Some block
        | None ->
          failwithf "Can't find cfg basic block labeled %d for linearid %d in %s\n"
            label dbg.line dbg.file ()

  let record_intra (from_loc:Loc.t) (to_loc:Loc.t) t count mispredicts func cfg =
    let from_block = get_block from_loc cfg in
    let to_block = get_block to_loc cfg in
    match from_block, to_block with
    | None, None ->
      if !verbose then
        printf "Ignore intra branch count %Ld from 0x%Lx to 0x%Lx, can't map to CFG\n"
          count from_loc.addr to_loc.addr;
    | Some from_block, None ->
      if !verbose then
        printf "Ignore intra branch count %Ld from 0x%Lx to 0x%Lx, \
                can't map target to CFG\n"
          count from_loc.addr to_loc.addr;
      Func.record func ~label:from_block.start ~count:count
    | None, Some to_block ->
      if !verbose then
        printf "Ignore intra branch count %Ld from 0x%Lx to 0x%Lx, \
                can't map source to CFG\n"
          count from_loc.addr to_loc.addr;
      Func.record func ~label:to_block.start ~count:count
    | Some from_block, Some to_block ->
      Func.record func ~label:from_block.start ~count:count;
      Func.record func ~label:to_block.start ~count:count;
      let from_linearid = get_linearid t from_loc in
      let to_linearid = get_linearid t to_loc in
      let bi = Func.get_block_info func from_block.start in
      let b = { Block_info.
                target=to_loc;
                target_label=Some to_block.start;
                intra=true;
                taken=count;
                mispredicts;
              } in
      Block_info.add_branch bi b;

      if from_block.terminator.id = from_linearid then begin
        (* Find the corresponding successor *)
        match from_block.terminator.desc with
        | Return  (* return from a recursive call *)
        | Tailcall _  (* tailcall  *)
        | Raise _ -> failwith "Not implemented"
        | _ -> failwith "Not implemented"
      end
      else begin
        (* recursive call, find the instruction *)
        failwith "Not implemented" (* find_call from_block *)
      end

  let record_exit (from_loc:Loc.t) (to_loc:Loc.t) t count mispredicts func cfg =
    (* Branch going outside of this function. *)
    match get_block from_loc cfg with
    | None ->
      if !verbose then
        printf "Ignore inter branch count %Ld from 0x%Lx. Can't map to CFG.\n"
          count from_loc.addr;
    | Some from_block ->
      Func.record func ~label:from_block.start ~count:count;
      (* Find the corresponding instruction and update its counters.
         The instruction is either a terminator or a call.*)
      let linearid = get_linearid t from_loc in
      let terminator = from_block.terminator in
      let bi = Func.get_block_info func from_block.start in
      if terminator.id = linearid then begin
        (* terminator *)
        match terminator.desc with
        | Branch _ | Switch _ | Tailcall _  ->
          (* can't branch outside the current function *)
          assert false;
        | Return | Raise _ ->
          let b = { Block_info.
                    target=to_loc;
                    target_label=None;
                    intra=false;
                    taken=count;
                    mispredicts;
                  } in
          Block_info.add_branch bi b;
      end else begin
        (* Call *)
        match get_basic_instr linearid from_block with
        | None -> assert false; (* we've checked before for presence of dbg info *)
        | Some instr ->
          match instr.desc with
          | Call cop ->
            let b = { Block_info.
                      target=to_loc;
                      target_label=None;
                      intra=false;
                      taken=count;
                      mispredicts;
                    } in
            Block_info.add_call bi from_loc b
          | _ -> assert false
      end

  let record_entry (from_loc:Loc.t) (to_loc:Loc.t) t count mispredicts func cfg  =
    (* Branch into this function from another function,
       which may be unknown. One of the following situations:
       Callee: branch target is the first instr in the entry block.
       Return from call: branch target is a label after the call site.
       Exception handler: branch target is a trap handler.
    *)
    match get_block to_loc cfg with
    | None ->
      if !verbose then
        printf "Ignore inter branch count %Ld to 0x%Lx. Can't map to CFG.\n"
          count to_loc.addr;
    | Some to_block ->
      Func.record func ~label:to_block.start ~count:count;
      (* Find the corresponding instruction and update its counters.*)
      let linearid = get_linearid t to_loc in
      let bi = Func.get_block_info func to_block.start in
      let first_instr_linearid =
        match to_block.body with
        | [] -> to_block.terminator.id
        | hd::tl -> hd.id in
      (* We could infer call targets from this info, but its not implemented
         yet because we don't have much use for it and it would
         change the way Block_info.add_call maps callsites using locations
         because we don't necessarily have a location for the caller.
         Also note that add_call currently checks that each callee can
         be installed at most once, as guaranteed by the aggregated profile. *)
      if first_instr_linearid = linearid &&
         (to_block.start = Cfg_builder.entry_label cfg
          (* Callee *)
          || Cfg_builder.is_trap_handler cfg to_block.start
          (* Exception handler *)
         )
      then begin
        (*  Block_info of potential callers can be updated *)
      end else begin
        (* Return from a call. Find predecessor instruction
           and check that it is a call.
           There could be more than one predecessor.
           We have enough information to find which one in some cases. *)
        let record_call (instr:Cfg.basic Cfg.instruction) =
          match instr.desc with
          | Call _ -> ()
          | _ -> assert false
        in
        let find_prev_block_call () =
          (* find a predecessor block that must end in a call
             and fallthrough. *)
          Cfg.LabelSet.iter
            (fun pred_label ->
              let pred = Option.value_exn (Cfg_builder.get_block cfg pred_label) in
              match pred.terminator.desc with
              | Branch [(Always,label)] when label=to_block.start ->
                let last_instr = List.last_exn to_block.body in
                record_call last_instr;
              | _ -> assert false)
            to_block.predecessors
        in
        if first_instr_linearid = linearid then begin
          find_prev_block_call ()
        end else begin
          match prev_instr linearid to_block with
          | Some instr ->
            record_call instr;
          | None -> (* instr with linearid must be the terminator *)
            assert (to_block.terminator.id = linearid);
            match to_block.body with
            | [] ->
              (* empty body means the call was in one of the pred blocks *)
              find_prev_block_call ()
            | _ ->
              let last_instr = List.last_exn to_block.body in
              record_call last_instr
        end
      end


  (* Depending on the settings of perf record and the corresponding CPU
     configuration, LBR may capture different kinds of branches,
     including function calls and returns. *)
  let record_branch t from_loc to_loc count mispredicts (func:Func.t) cfg =
    let open Loc in
    (* at least one of the locations is known to be in this function *)
    match from_loc.rel, to_loc.rel with
    | Some from_rel, Some to_rel
      when from_rel.id = func.id && to_rel.id = func.id ->
      record_intra from_loc to_loc t count mispredicts func cfg
    | Some from_rel, _ when (from_rel.id = func.id) ->
      record_exit from_loc to_loc t count mispredicts func cfg
    | _, Some to_rel when to_rel.id = func.id ->
      record_entry from_loc to_loc t count mispredicts func cfg
    | _ -> assert false

  let record_trace t from_addr to_addr count func cfg =
    failwith "Not implemented"

  (* Translate linear ids of this function's locations to cfg labels
     within this function, find the corresponding basic blocks
     and update their block_info.
     Perform lots of sanity checks to make sure the location of the
     execounts match the instructions in the cfg. *)
  let compute_execounts t func cfg =
    (* Associate instruction counts with basic blocks *)
    Hashtbl.iteri func.agg.instructions ~f:(fun ~key ~data ->
      let loc = get_loc t key in
      match get_block loc cfg with
      | None ->
        if !verbose then
          printf "Ignore exec count at 0x%Lx\n, can't map to cfg\n" loc.addr
      | Some block ->
        Func.record func ~label:block.start ~count:data
    );
    (* Associate fall-through trace counts with basic blocks *)
    Hashtbl.iteri func.agg.traces ~f:(fun ~key ~data ->
      let (from_addr,to_addr) = key in
      let from_loc = get_loc t from_addr in
      let to_loc = get_loc t to_addr in
      record_trace t from_loc to_loc data func cfg
    );
    (* Associate branch counts with basic blocks *)
    Hashtbl.iteri func.agg.branches ~f:(fun ~key ~data ->
      let mispredicts = Hashtbl.find_exn func.agg.mispredicts key in
      let (from_addr,to_addr) = key in
      let from_loc = get_loc t from_addr in
      let to_loc = get_loc t to_addr in
      record_branch t from_loc to_loc data mispredicts func cfg
    )
    (* CR gyorsh: propagate counts to compute missing fallthroughs? *)


  (* Compute detailed execution counts for function [name] using its CFG *)
  let compute_cfg_execounts t name cfg =
    match Hashtbl.find t.name2id name with
    | None ->
      if !verbose then
        printf "Not found profile for %s with cfg.\n" name;
      None
    | Some id ->
      let func = Hashtbl.find_exn t.functions id in
      if func.count > 0L && func.has_linearids then begin
        compute_execounts t func cfg;
        Some func.blocks
      end else
        None

  (* Partition aggregated_perf to functions and calculate total execution counts
     of each function.
     Total execution count of a function is determined from the execution counts
     of samples contained in this function. It uses LBR info:
     if a branch source or target is contained in the function,
     it contributes to execution count of the function.
     It does not use the CFG.
     In particular, it does not count instructions that can be traced using LBR.
     The advantage is that we can compute it for non-OCaml functions. *)
  let create_func_execounts t (agg:Aggregated_perf.t) =
    Hashtbl.iteri agg.instructions
      ~f:(fun ~key ~data ->
        match get_func t key with
        | None -> ()
        | Some func ->
          func.count <- Int64.(func.count + data);
          Hashtbl.add_exn func.agg.instructions ~key ~data);

    let process (from_addr, to_addr) update =
      match get_func t from_addr, get_func t to_addr with
        | None, None -> ()
        | None, Some to_func -> update to_func
        | Some from_func, None -> update from_func
        | Some from_func, Some to_func ->
          if from_func = to_func then begin
            update to_func
          end else begin
            (* interprocedural branch: add to both functions *)
            update from_func;
            update to_func
          end
    in
    Hashtbl.iteri agg.branches
      ~f:(fun ~key ~data ->
        let mispredicts = Hashtbl.find_exn agg.mispredicts key in
        let update_br func =
          func.count <- Int64.(func.count + data);
          Hashtbl.add_exn func.agg.branches ~key ~data;
          Hashtbl.add_exn func.agg.mispredicts ~key ~data:mispredicts in
        process key update_br);
    Hashtbl.iteri agg.traces
      ~f:(fun ~key ~data ->
        (* traces don't contribute to func's total count because it
           is account for in branches. *)
        let update_tr func =
          Hashtbl.add_exn func.agg.traces ~key ~data in
        process key update_tr)

  (* Find or add the function and return its id *)
  let get_func_id t ~name ~start =
    match Hashtbl.find t.name2id name with
    | None ->
      let id = Hashtbl.length t.functions in
      let func = Func.mk ~id ~name ~start in
      Hashtbl.add_exn t.functions ~key:id ~data:func;
      Hashtbl.add_exn t.name2id ~key:name ~data:id;
      func.id
    | Some id ->
      let func = Hashtbl.find_exn t.functions id in
      assert (func.id = id);
      assert (func.name = name);
      assert (func.start = start);
      func.id

  let decode_loc t locations addr =
    let open Loc in
    match Elf_locations.resolve_function_containing locations
            ~program_counter:addr with
    | None ->
      if !verbose then
        printf "Cannot find function symbol containing 0x%Lx\n" addr;
      { addr; rel=None; dbg=None; }
    | Some interval ->
      let name = interval.v in
      let start = interval.l in
      let offset =
        match Int64.(to_int (addr - start)) with
        | None -> failwithf "Offset too big: 0x%Lx" (Int64.(addr - start)) ()
        | Some offset -> assert (offset >= 0); offset in
      let id = get_func_id t ~name ~start in
      let rel = Some { id; offset; } in
      let dbg =
        match Ocaml_locations.(decode_line locations
                                 ~program_counter:addr name Linearid) with
        | None -> None
        | Some (file,line) ->
          (* Set has_linearids of this function *)
          let func = Hashtbl.find_exn t.functions id  in
          func.has_linearids <- true;
          Some {Loc.file;line;}
      in
      { addr; rel; dbg; }

  let decode_aggregated locations (aggregated_perf:Aggregated_perf.t) =
    if !verbose then
      printf "Decoding perf profile.\n";

    (* Collect all addresses that need decoding. *)
    (* Mispredicts and traces use the same addresses as branches,
       so no need to add them *)
    let len = Hashtbl.length aggregated_perf.instructions +
              Hashtbl.length aggregated_perf.branches in
    (* Elf_locations does not use Core, so we need to create Caml.Hashtbl *)
    let addresses = Caml.Hashtbl.create len in
    let add key =
      if not (Caml.Hashtbl.mem addresses key) then
        Caml.Hashtbl.add addresses key ();
    in
    let add2 (fa,ta) = add fa; add ta in
    Hashtbl.iter_keys aggregated_perf.instructions ~f:add;
    Hashtbl.iter_keys aggregated_perf.branches ~f:add2;
    (* A key may appear in both t.instruction and t.branches *)
    let size = Caml.Hashtbl.length addresses in
    assert (size <= len);
    (* Resolve and cache all addresses we need in one pass over the binary. *)
    Elf_locations.resolve_all locations addresses ~reset:true;
    (* Decode all locations: map addresses to locations *)
    let t = mk size in
    Caml.Hashtbl.iter
      (fun addr _ ->
         let loc = decode_loc t locations addr in
         Hashtbl.add_exn t.addr2loc ~key:addr ~data:loc)
      addresses;
    create_func_execounts t aggregated_perf;
    t

  let read filename =
    if !verbose then
      printf "Reading aggregated decoded profile from %s\n" filename;
    let t =
      match Parsexp_io.load (module Parsexp.Single) ~filename with
      | Ok t_sexp -> t_of_sexp t_sexp
      | Error error ->
        Parsexp.Parse_error.report Caml.Format.std_formatter error ~filename;
        failwith "Cannot parse aggregated decoded profile file"
    in
    if !verbose then begin
      Printf.printf !"Aggregated decoded profile:\n%{sexp:t}\n" t;
    end;
    t

  let write t filename =
    if !verbose then
      printf "Writing aggregated decoded profile to %s\n" filename;
    let chan = Out_channel.create filename in
    Printf.fprintf chan !"%{sexp:t}\n" t;
    Out_channel.close chan

  let write_top_functions t filename =
    if !verbose then
      printf "Writing top functions to %s\n" filename;
    (* Sort functions using preliminary function-level execution counts
       in descending order. *)
    let sorted = List.sort (Hashtbl.data t.functions) ~compare:Func.compare in
    let fl = List.map sorted ~f:(fun func -> func.name) in
    Layouts.Func_layout.write_linker_script fl filename

  let write_bolt t filename =
    let open Loc in
    if !verbose then
      printf "Writing aggregated decoded prfile in bolt form to %s\n" filename;
    let chan = Out_channel.create filename in
    let write_bolt_count ~key ~data =
      let mis = Option.value
                  (Hashtbl.find agg.mispredicts key)
                  ~default:0L in
      let (from_addr, to_addr) = key in
      let from_loc = get_loc t from_addr  in
      let to_loc = get_loc t to_addr in
      match from_loc.func, to_loc.func with
      | None, None -> ()
      | _ ->
        Printf.fprintf chan "%s %s %Ld %Ld\n"
          (to_bolt_string from_loc)
          (to_bolt_string to_loc)
          mis
          data in
    Hashtbl.iteri t.counts.branches ~f:write_bolt_count;
    Out_channel.close chan

end

module Perf = struct
  (* Perf profile format as output of perf script -F pid,ip,brstack *)

  type mispredict_flag = M | P | NOT_SUPPORTED
  [@@deriving compare, sexp]

  let mispredicted = function
    | M -> true
    | _ -> false

  type br = {
    from_addr : Addr.t;
    to_addr : Addr.t;
    mispredict : mispredict_flag;
    index : int; (* Position on the stack, with 0 being the most recent branch.
                    This field is only used for only for validation. *)
    (* cycles : int; *)
  } [@@deriving compare, sexp]

  type sample = {
    ip: Addr.t;  (* instruction pointer where the sample was taken *)
    brstack: br list; (* branch stack is the last branch record (LBR) *)
  } [@@deriving compare, sexp]

  type t = sample list
  [@@deriving compare, sexp]

  let parse_br index s =
    match String.split s ~on:'/' with
    | [from_addr;to_addr;m;t;a;c] ->
      let mispredict = match m with
        | "M" -> M
        | "P" -> P
        | "-" -> NOT_SUPPORTED
        | _ -> failwithf "Cannot parse mispredict flag %s in %s" m s ()
      in
      (* Parse and ignore t and a flags. *)
      begin match t with
      | "X" | "-" -> ()
      | _ -> failwithf "Cannot parse mispredict flag %s in %s" m s ()
      end;
      begin match a with
      | "A" | "-" -> ()
      | _ -> failwithf "Cannot parse mispredict flag %s in %s" a s ()
      end;
      (* Parse and ignore cycles. CR gyorsh: use for optimizations. *)
      let _cycles = Int.of_string c in
      {
        from_addr = Int64.of_string from_addr;
        to_addr = Int64.of_string to_addr;
        index;
        mispredict;
      }
    | _ -> failwithf "Cannot parse %s\n" s ()

  let print t =
    Printf.printf !"%{sexp:t}\n" t

  (* The most recent branch is printed first by perf script.
     The number of branch entries vary based on the underlying hardware.
     This function reverses the stack from its perf profile order. *)
  let rec parse_brstack (index,brstack) row =
    match row with
    | [] -> (index,brstack)
    | hd::tl ->
      let brstack = (parse_br index hd)::brstack
      in parse_brstack (index+1,brstack) tl

  let split_on_whitespace row =
    let r = String.split ~on:' ' row in
    List.filter r ~f:(fun s -> not (String.is_empty s))

  let hex s =
    if String.is_prefix ~prefix:"0x" s then
      s
    else
      "0x" ^ s

  let row_to_sample ~keep_pid row =
    match split_on_whitespace row with
    | pid::ip::rest ->
      let pid = Int.of_string pid in
      if keep_pid pid then begin
        if !verbose then
          printf "parsing ip %s\n" ip;
        let sample =  {
          ip = Int64.of_string (hex ip);
          brstack = snd (parse_brstack (0,[]) rest);
        } in
        Some sample
      end
      else
        None
    | _ -> failwithf "Cannot parse %s\n" row ()

  let pids = ref Int.Set.empty

  let check_keep_pid ?expected_pid p =
    if !verbose then begin
      if not (Int.Set.mem !pids p) then begin
        printf "Found new pid: %d\n" p;
        pids := Int.Set.add !pids p;
      end
    end;
    match expected_pid with
    | None -> true
    | Some expected_pid ->
      if expected_pid = p then true
      else begin
        if !verbose then
          printf "Mismatch pid: expected %L found %L\n" expected_pid p;
        false
      end

  let read ?(expected_pid=None) filename =
    if !verbose then
      printf "Reading perf profile generated by \
              \"perf script -F pid,ip,brstack\" from %s\n" filename;
    let chan = In_channel.create filename in
    let keep_pid = check_keep_pid ?expected_pid in
    let t = In_channel.fold_lines chan ~init:[]
              ~f:(fun acc row ->
                match row_to_sample ~keep_pid row with
                | None -> acc
                | Some sample -> sample::acc) in
    In_channel.close chan;
    if !verbose then begin
      print t;
      Printf.printf "Found pids:\n";
      Int.Set.iter !pids ~f:(fun pid -> printf "%d\n" pid)
    end;
    t


  type stats = {
    ignored:int;
    total:int;
    lbr:int;
  }
  let read_and_aggregate ?(expected_pid=None) filename =
    if !verbose then
      printf "Aggregate perf profile from %s.\n" filename;

    let inc table key =
      Hashtbl.update table key
        ~f:(fun v -> Int64.(1L + Option.value ~default:0L v)) in
    let aggregated = Aggregated_perf.empty in
    (* CR gyorsh: aggregate during parsing of perf profile *)
    let aggregate sample =
      inc aggregated.instructions sample.ip;
      List.iter sample.brstack
        ~f:(fun br ->
          let key = (br.from_addr,br.to_addr) in
          inc aggregated.branches key;
          if mispredicted br.mispredict then
            inc aggregated.mispredicts key;
        );
      (* Instructions executed between branches can be inferred from brstack *)
      ignore (List.fold sample.brstack ~init:None
                ~f:(fun prev cur ->
                  match prev with
                  | None -> Some cur
                  | Some prev ->
                    assert (prev.index = (cur.index + 1));
                    let from_addr = prev.to_addr in
                    let to_addr = cur.from_addr in
                    let key = (from_addr,to_addr) in
                    inc aggregated.traces key;
                    Some cur
                ): br option)

    in
    let chan = In_channel.create filename in
    let keep_pid = check_keep_pid ?expected_pid in
    let empty_stats = {ignored=0;total=0;lbr=0} in
    let stats = In_channel.fold_lines chan ~init:empty_stats
      ~f:(fun stats row ->
        match row_to_sample ~keep_pid row with
        | None ->
          { stats with ignored=stats.ignored+1; total=stats.total+1 }
        | Some sample ->
          aggregate sample;
          { stats with total=stats.total+1;
                       lbr=stats.lbr+(List.length sample.brstack) }
         ) in
    In_channel.close chan;
    if !verbose then begin
      Printf.printf "Read %d samples with %d LBR entries\n" stats.total stats.lbr;
      let r = Float.( (of_int stats.ignored) * 100.0 / (of_int stats.total) ) in
      Printf.printf "Ignored %d samples (%.1f)\n" stats.ignored r;
      Printf.printf "Found pids:\n";
      Int.Set.iter !pids ~f:(fun pid -> printf "%d\n" pid)
    end;
      aggregated

end

