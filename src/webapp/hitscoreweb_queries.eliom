
(* WARNING: Do not open Hitscore_std, Core.Std, etc ... PGOCaml hates it. *)

module Flow = Hitscoreweb_std.Hitscore_lwt.Flow
module PGOCaml = Hitscoreweb_std.PGOCaml
module Layout = Hitscoreweb_std.Hitscore_lwt.Layout

let full_libraries dbh =
  let query () =
    PGSQL (dbh) "nullable-results"
      "SELECT stock.g_id, stock.name, stock.project,
                stock.description, stock.application,
                stock.stranded, stock.truseq_control, stock.rnaseq_control,
                stock.barcode_type, stock.barcodes, stock.custom_barcodes,
                stock.p5_adapter_length, stock.p7_adapter_length, stock.note,
                sample.name, organism.name,
                person.email, protocol.name FROM 
         ((stock_library stock LEFT OUTER JOIN 
             (sample LEFT OUTER JOIN organism ON sample.organism = organism.g_id) 
             ON sample.g_id = stock.sample)
           LEFT OUTER JOIN person ON stock.preparator = person.g_id)
         LEFT OUTER JOIN protocol ON stock.protocol = protocol.g_id
         ORDER BY stock.name"
  in
  Flow.wrap_pgocaml ~query ~on_result:Flow.return
   
let library_submissions ~lib_id dbh =
  let query () =
    PGSQL (dbh)
      "SELECT flowcell.serial_name, lane.g_id, lane.contacts FROM
        input_library input, lane, flowcell
        WHERE lane.libraries @> array_append ('{}', input.g_id)
        AND flowcell.lanes @> array_append ('{}', lane.g_id)
        AND input.library = $lib_id"
  in
  Flow.wrap_pgocaml ~query ~on_result:Flow.return

let sample_sheet_kind ~dbh sample_sheet =
  let sample_sheet = sample_sheet.Layout.Record_sample_sheet.id in
  let query () =
    PGSQL (dbh)
      "SELECT assemble_sample_sheet.kind
       FROM assemble_sample_sheet, sample_sheet
       WHERE assemble_sample_sheet.g_result = $sample_sheet"
  in
  Flow.(
    wrap_pgocaml ~query ~on_result:return
    >>= function
    | kind_str :: _ ->
      begin match Layout.Enumeration_sample_sheet_kind.of_string kind_str with
          | Core.Std.Result.Ok k -> return k
          | Core.Std.Result.Error e -> error (`sample_sheet_kind_of_string e)
      end
    | _ -> error (`sample_sheet_kind_not_found sample_sheet))

let delivered_unaligned_directories_of_lane ~dbh lane_pointer =
  let lane_id = lane_pointer.Layout.Record_lane.id in
  let query () =
    PGSQL (dbh)
      "SELECT bcl_to_fastq_unaligned.directory
       FROM prepare_unaligned_delivery, invoicing,
            bcl_to_fastq_unaligned
       WHERE g_status = 'Succeeded'
         AND unaligned = bcl_to_fastq_unaligned.g_id
         AND invoice = invoicing.g_id
         AND invoicing.lanes @> array_append ('{}', $lane_id :: int);" in
  Hitscoreweb_std.(
    wrap_pgocaml ~query ~on_result:(fun l ->
      List.dedup l
      |! List.map ~f:Layout.File_system.unsafe_cast
      |! return))

let fastx_stats_of_unaligned_volume ~dbh unaligned_pointer =
  let link_hack =
    Printf.sprintf "(Link ((id %ld)))" unaligned_pointer.Layout.File_system.id in
  Printf.eprintf "looking for %s\n%!" link_hack;
  let query () =
    PGSQL (dbh)
     "SELECT fr.directory
      FROM fastx_quality_stats_result as fr,
           fastx_quality_stats as fq,
           generic_fastqs,
           g_volume
      WHERE
        fr.g_id = fq.g_result
        AND fq.input_dir = generic_fastqs.g_id
        AND generic_fastqs.directory = g_volume.g_id
        AND fq.filter_names = '(*.fastq *.fastq.gz)'
        AND g_volume.g_sexp = $link_hack
      ORDER BY fr.g_last_modified" in
  Hitscoreweb_std.(
    wrap_pgocaml ~query ~on_result:(fun l ->
      List.last l
      |! Option.map ~f:Layout.File_system.unsafe_cast
      |! return))
