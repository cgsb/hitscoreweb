
(* WARNING: Do not open Hitscore_std, Core.Std, etc ... PGOCaml hates it. *)

module RIO = Hitscoreweb_std.Hitscore_lwt.Result_IO
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
  RIO.wrap_pgocaml ~query ~on_result:RIO.return
   
let library_submissions ~lib_id dbh =
  let query () =
    PGSQL (dbh)
      "SELECT flowcell.serial_name, lane.g_id, lane.contacts FROM
        input_library input, lane, flowcell
        WHERE lane.libraries @> array_append ('{}', input.g_id)
        AND flowcell.lanes @> array_append ('{}', lane.g_id)
        AND input.library = $lib_id"
  in
  RIO.wrap_pgocaml ~query ~on_result:RIO.return

let sample_sheet_kind ~dbh sample_sheet =
  let sample_sheet = sample_sheet.Layout.Record_sample_sheet.id in
  let query () =
    PGSQL (dbh)
      "SELECT assemble_sample_sheet.kind
       FROM assemble_sample_sheet, sample_sheet
       WHERE assemble_sample_sheet.g_result = $sample_sheet"
  in
  RIO.(
    wrap_pgocaml ~query ~on_result:return
    >>= function
    | kind_str :: _ ->
      begin match Layout.Enumeration_sample_sheet_kind.of_string kind_str with
          | Core.Std.Result.Ok k -> return k
          | Core.Std.Result.Error e -> error (`sample_sheet_kind_of_string e)
      end
    | _ -> error (`sample_sheet_kind_not_found sample_sheet))

