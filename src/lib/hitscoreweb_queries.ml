
(* WARNING: Do not open Hitscore_std, Core.Std, etc ... PGOCaml hates it. *)

module Make 
  (RIO : Hitscore_interfaces.RESULT_IO)
  (PGOCaml : PGOCaml_generic.PGOCAML_GENERIC with type 'a monad = 'a RIO.IO.t)
  = struct


  let full_libraries dbh =
    let query () =
      PGSQL (dbh) "nullable-results"
        "SELECT stock.g_id, stock.name, stock.project,
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
        "SELECT flowcell.serial_name, lane.g_id FROM
        input_library input, lane, flowcell
        WHERE lane.libraries @> array_append ('{}', input.g_id)
        AND flowcell.lanes @> array_append ('{}', lane.g_id)
        AND input.library = $lib_id"
    in
    RIO.wrap_pgocaml ~query ~on_result:RIO.return


end
