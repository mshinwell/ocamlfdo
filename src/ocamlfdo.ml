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
open Layouts

let verbose = ref false

let strict = ref false

let time f x =
  let open Time in
  let start = now () in
  let fx = f x in
  let stop = now () in
  Printf.printf "Execution time: %s\n"
    (Span.to_string (abs_diff stop start)) ;
  fx

let load_locations binary_filename =
  let elf_locations =
    Elf_locations.create ~elf_executable:binary_filename
  in
  if !verbose then Elf_locations.print_dwarf elf_locations ;
  elf_locations

let setup_reorder ~binary_filename ~perf_profile_filename
    ~raw_layout_filename ~rel_layout_filename ~linearid_layout_filename
    ~random_order ~gen_linearid_layout ~gen_linearid_profile
    ~linearid_profile_filename ~gen_bolt_fdata =
  if random_order then
    (* let random_state = Random.State.make [ deterministic seed ]; *)
    Reorder.Random Random.State.default
  else
    match binary_filename with
    | None -> (
      match rel_layout_filename with
      | Some rel_layout_filename ->
          let layout =
            convert_layout (Rel_layout.read rel_layout_filename)
          in
          Reorder.Cfg layout
      | None -> (
        match linearid_layout_filename with
        | Some linearid_layout_filename ->
            let layout =
              convert_layout (Rel_layout.read linearid_layout_filename)
            in
            Reorder.Linear layout
        | None -> Reorder.Identity ) )
    | Some binary_filename -> (
        let locations = load_locations binary_filename in
        match raw_layout_filename with
        | Some raw_layout_filename ->
            let raw_layout = Raw_layout.read raw_layout_filename in
            let writer = Rel_layout.writer gen_linearid_layout in
            let layout = decode_layout_all locations raw_layout writer in
            Reorder.Linear layout
        | None -> (
          match perf_profile_filename with
          | None -> (
            match linearid_profile_filename with
            | None -> Reorder.Identity
            | Some linearid_profile_filename ->
                let linearid_profile =
                  Profiles.Aggregated_decoded.read linearid_profile_filename
                in
                let config =
                  Reorder.Config.default linearid_profile_filename
                in
                Reorder.Profile (linearid_profile, config) )
          | Some perf_profile_filename ->
              (* CR gyorsh: decoding raw perf data should be separate from
                 reordering algorithm setup. *)
              (* CR gyorsh: check buildid of the samples. Use owee? *)
              (* CR gyorsh: Check pid of the samples. *)
              (* First aggregate raw profile and then decode it. *)
              let aggr_perf_profile =
                Profiles.Perf.read_and_aggregate perf_profile_filename
              in
              let linearid_profile =
                Profiles.Aggregated_decoded.decode_aggregated locations
                  aggr_perf_profile
              in
              let gen_linearid_profile =
                match gen_linearid_profile with
                | None -> perf_profile_filename ^ ".linearid"
                | Some f -> f
              in
              Profiles.Aggregated_decoded.write linearid_profile
                gen_linearid_profile ;
              let gen_bolt_fdata =
                match gen_bolt_fdata with
                | None -> gen_linearid_profile ^ ".fdata"
                | Some f -> f
              in
              Profiles.Aggregated_decoded.write_bolt linearid_profile
                aggr_perf_profile gen_bolt_fdata ;
              (* CR gyorsh: configuration shouldn't be hardcoded! *)
              let config =
                let open Reorder.Config in
                { (default gen_linearid_profile) with
                  reorder_functions= Reorder.Config.Execounts }
              in
              (* CR gyorsh: write_top is here just for testing! *)
              (* Do we ever need the cfg to decide on function order? *)
              Profiles.Aggregated_decoded.write_top_functions
                linearid_profile
                (Reorder.Config.linker_script_filename config "prelim") ;
              Reorder.Profile (linearid_profile, config) ) )

let call_ocamlopt args =
  (* Set debug "-g" to emit dwarf locations. *)
  Clflags.debug := true ;
  Clflags.extended_debug := true ;
  (* set command line to args to call ocamlopt *)
  ( match args with
  | None | Some [] ->
      if !verbose then printf "Missing compilation command\n"
  | Some args ->
      if !verbose then (
        printf "ocamlopt " ;
        List.iter ~f:(fun s -> printf " %s" s) args ;
        printf "\n" ) ) ;
  let args = Array.of_list (Option.value args ~default:[]) in
  let len = Array.length args in
  let argc = Array.length Sys.argv in
  assert (len < argc) ;
  Array.blit ~src:args ~src_pos:0 ~dst:Sys.argv ~dst_pos:1 ~len ;
  (* CR gyorsh: Can't resize args array. Fake missing arguments. Better way? *)
  Array.fill Sys.argv ~pos:(len + 1) ~len:(argc - len - 1) "-absname" ;
  (* -inlining-report? *)
  Optmain.main ()

let check_equal f ~new_body =
  let open Linearize in
  let rec equal i1 i2 =
    (* Format.kasprintf prerr_endline "@;%a" Printlinear.instr i1;
     * Format.kasprintf prerr_endline "@;%a" Printlinear.instr i2; *)
    if
      i1.desc = i2.desc && i1.id = i2.id
      && Reg.array_equal i1.arg i2.arg
      && Reg.array_equal i1.res i2.res
      && Reg.Set.equal i1.live i2.live
      && Debuginfo.compare i1.dbg i2.dbg = 0
    then if i1.desc = Lend then true else equal i1.next i2.next
    else (
      Format.kasprintf prerr_endline "Equality failed in %s on:@;%a@;%a"
        f.fun_name Printlinear.instr i1 Printlinear.instr i2 ;
      false )
  in
  if not (equal f.fun_body new_body) then (
    let name = X86_proc.string_of_symbol "" f.fun_name in
    (* Separate files for before and after to make it easier to diff *)
    Report.linear ~name "Before" f ;
    Report.linear ~name "After" {f with fun_body= new_body} ;
    if !strict then
      failwithf
        "Conversion from linear to cfg and back to linear is not an \
         identity function %s.\n"
        name () )

let print_linear msg f =
  if false then
    if !verbose then (
      Printf.printf "%s processing %s\n" f.Linearize.fun_name msg ;
      Format.kasprintf prerr_endline "@;%a" Printlinear.fundecl f )

let rec remove_discriminator = function
  | [] -> []
  | item :: t ->
      if String.is_suffix item.Debuginfo.dinfo_file ~suffix:".linear" then t
      else item :: remove_discriminator t

let rec remove_linear_discriminator i =
  let open Linearize in
  match i.desc with
  | Lend -> i
  | _ ->
      { i with
        dbg= remove_discriminator i.dbg
      ; next= remove_linear_discriminator i.next }

let remove_linear_discriminators f =
  let open Linearize in
  { f with
    fun_dbg= remove_discriminator f.fun_dbg
  ; fun_body= remove_linear_discriminator f.fun_body }

(* CR gyorsh: this is intended as a report at source level and human
   readable form, like inlining report. Currently, just prints the IRs.
   Could share infrastructure with inlining report to map back to source
   when reordering involves inlined functions. CR gyorsh: add separate
   "dump" flags for all passes in ocamlfdo, printing to stdout similarly to
   -dcmm -dlinear in the compiler, etc. *)
let write_reorder_report f cfg newf newcfg =
  if not (phys_equal cfg newcfg) then (
    (* Separate files for before and after make it easier to diff *)
    let name = X86_proc.string_of_symbol "" f.Linearize.fun_name in
    Report.linear ~name "Before-Reorder" f ;
    Report.linear ~name "After-Reorder" newf ;
    Report.cfg ~name "Before-Reorder" cfg ;
    Report.cfg ~name "After-Reorder" newcfg )

let main ~binary_filename ~perf_profile_filename ~raw_layout_filename
    ~rel_layout_filename ~linearid_layout_filename ~gen_rel_layout
    ~gen_linearid_layout ~random_order ~eliminate_dead_blocks
    ~eliminate_fallthrough_blocks ~remove_linear_ids ~reorder_report
    ~preserve_orig_labels ~gen_linearid_profile ~linearid_profile_filename
    ~gen_bolt_fdata args =
  let algo =
    setup_reorder ~binary_filename ~perf_profile_filename
      ~raw_layout_filename ~rel_layout_filename ~linearid_layout_filename
      ~random_order ~gen_linearid_layout ~gen_linearid_profile
      ~linearid_profile_filename ~gen_bolt_fdata
  in
  let reorder = Reorder.reorder ~algo in
  let w_rel = Rel_layout.writer gen_rel_layout in
  let write_rel_layout new_cfg =
    let new_layout = Cfg_builder.get_layout new_cfg in
    let fun_name = Cfg_builder.get_name new_cfg in
    w_rel fun_name new_layout
  in
  let validate f ~new_body =
    match algo with
    | Reorder.Identity ->
        if
          eliminate_fallthrough_blocks || eliminate_dead_blocks
          || not preserve_orig_labels
        then ()
        else check_equal f ~new_body
    | _ -> ()
  in
  let transform f =
    print_linear "Before" f ;
    let cfg = Cfg_builder.from_linear f ~preserve_orig_labels in
    let new_cfg = reorder cfg in
    write_rel_layout new_cfg ;
    (* If we eliminate dead blocks before a transformation then some
       algorithms might not apply because they rely on perf data based on
       original instructions. *)
    if eliminate_fallthrough_blocks then
      (* implies dead block elimination *)
      Cfg_builder.eliminate_fallthrough_blocks new_cfg
    else if eliminate_dead_blocks then
      Cfg_builder.eliminate_dead_blocks new_cfg ;
    let new_body = Cfg_builder.to_linear new_cfg in
    validate f ~new_body ;
    let fnew = {f with fun_body= new_body} in
    if reorder_report then write_reorder_report f cfg fnew new_cfg ;
    let fnew =
      if remove_linear_ids then remove_linear_discriminators fnew else fnew
    in
    print_linear "After" fnew ; fnew
  in
  Reoptimize.setup ~f:transform ;
  let finish () = Reorder.finish algo ; Report.finish () in
  at_exit finish ; call_ocamlopt args ; finish ()

let command =
  Command.basic ~summary:"Feedback-directed optimizer for Ocaml"
    ~readme:(fun () ->
      "\n\
       Build your program with ocamlfdo to enable extra debug info\n\
       for low-level optimizations (currently, only linearize pass):\n\
       $ ocamlfdo -- <standard ocamlopt options including -o prog.exe>\n\n\
       Collect perf profile with LBR information:\n\
       $ perf record -e cycles:u -j any,u -o perf.data <prog.exe> <args..>\n\n\
       Run ocamlfdo with exactly the same version of the source code and\n\
       options as above:\n\
       $ ocamlfdo -perf-profile <perf.data> -binary <prog.exe> -- \\\n\
      \           <standard ocamlopt options including -o prog.fdo.exe>\n\n\
       Important: ocamlfdo relies on compiler-libs and thus the build of \
       ocamlfdo must\n\
       match the build of ocamlopt used above.\n" )
    Command.Let_syntax.(
      let%map_open v =
        flag "-verbose" ~aliases:["-v"] no_arg ~doc:" verbose"
      and q = flag "-q" no_arg ~doc:" quiet"
      and remove_linear_ids =
        flag "-remove-linear-ids" no_arg
          ~doc:" remove extra dwarf info with linear ids"
      and eliminate_dead_blocks =
        flag "-edb" no_arg ~doc:" eliminate dead blocks"
      and eliminate_fallthrough_blocks =
        flag "-efb" no_arg
          ~doc:" eliminate fallthrough blocks, implies -edb"
      and preserve_orig_labels =
        flag "-preserve-orig-labels" no_arg
          ~doc:" do not eliminate labels (for validation)"
      and gen_rel_layout =
        flag "-gen-layout"
          (optional Filename.arg_type)
          ~doc:"filename generate relative layout and write to <filename>"
      and gen_linearid_layout =
        flag "-gen-linearid-layout"
          (optional Filename.arg_type)
          ~doc:"filename generate relative layout and write to <filename>"
      and random_order =
        flag "-random-order" no_arg ~doc:" reorder blocks at random"
      and binary_filename =
        flag "-binary"
          (optional Filename.arg_type)
          ~doc:"filename elf binary to optimize"
      and perf_profile_filename =
        flag "-perf-profile"
          (optional Filename.arg_type)
          ~doc:"perf.data output of perf record"
      and gen_linearid_profile =
        flag "-gen-linearid-profile"
          (optional Filename.arg_type)
          ~doc:"filename output decoded perf profile"
      and gen_bolt_fdata =
        flag "-gen-bolt-fdata"
          (optional Filename.arg_type)
          ~doc:
            "filename output aggregated perf profile in bolt fdata format"
      and linearid_profile_filename =
        flag "-linearid-profile"
          (optional Filename.arg_type)
          ~doc:"filename use decoded perf profile"
      and raw_layout_filename =
        flag "-raw-layout"
          (optional Filename.arg_type)
          ~doc:"filename block layout for reordering: raw binary addresses"
      and rel_layout_filename =
        flag "-rel-layout"
          (optional Filename.arg_type)
          ~doc:
            "filename block layout for reordering relative to function \
             start,does not require -binary"
      and linearid_layout_filename =
        flag "-linearid-layout" ~aliases:["-layout"]
          (optional Filename.arg_type)
          ~doc:
            "filename same as -rel-layout but uses linear id not cfg labels"
      and reorder_report =
        flag "-reorder-report" no_arg
          ~doc:" Emit files showing the decisions"
      and args =
        flag "--" escape
          ~doc:"ocamlopt_args standard options passed to opcamlopt"
      in
      if v then verbose := true ;
      if q then verbose := false ;
      if !verbose then Report.verbose := true ;
      (* CR gyorsh: use choose_one to reduce the mess below? *)
      if !verbose then (
        if
          preserve_orig_labels
          && (eliminate_dead_blocks || eliminate_fallthrough_blocks)
        then (
          Printf.printf "Warning: Ignoring -preserve-orig-labels.\n" ;
          Printf.printf "Incompatible with -edb and -efb\n" ) ;
        if random_order then
          if
            (not (perf_profile_filename = None))
            || (not (raw_layout_filename = None))
            || (not (rel_layout_filename = None))
            || (not (linearid_layout_filename = None))
            || not (binary_filename = None)
          then (
            Printf.printf
              "Warning: Ignoring -perf-profile -raw-layout -layout \
               -linearid-layout -binary. " ;
            Printf.printf "Incompatible with -random\n" ) ;
        if binary_filename = None then (
          if
            (not (perf_profile_filename = None))
            || not (raw_layout_filename = None)
          then (
            Printf.printf
              "Warning: ignoring -raw_layout and -perf-profile. " ;
            Printf.printf "Cannot use without -binary.\n" ) )
        else if perf_profile_filename = None && raw_layout_filename = None
        then (
          Printf.printf "Warning: Ignoring -binary. " ;
          Printf.printf "Cannot use without -perf-profile or -raw-layout.\n" ) ) ;
      fun () ->
        main ~binary_filename ~perf_profile_filename ~raw_layout_filename
          ~rel_layout_filename ~linearid_layout_filename ~gen_rel_layout
          ~gen_linearid_layout ~random_order ~eliminate_dead_blocks
          ~eliminate_fallthrough_blocks ~remove_linear_ids ~reorder_report
          ~preserve_orig_labels ~gen_linearid_profile
          ~linearid_profile_filename ~gen_bolt_fdata args)

let () = Command.run command
