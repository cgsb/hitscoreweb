open Hitscoreweb_std



module Services = struct

  let default =
    Eliom_services.service
      ~path:[""] ~get_params:Eliom_parameters.unit ()

end

let flowcells hsc =
  Hitscore_lwt.db_connect hsc
  >>= fun dbh ->
  Layout.Record_flowcell.(
    get_all ~dbh
    >>= fun flowcells ->
    of_list_sequential flowcells ~f:(fun f ->
      cache_value ~dbh f >>| get_fields
      >>= fun {serial_name; lanes} ->
      return Html5.(li [pcdata serial_name]))
    >>= fun ul ->
    return (List.length flowcells, ul)
  )
  >>= fun (length, items) ->
  return Html5.(div [
    h1 [ ksprintf pcdata "%d Flowcells" length];
    ul items
  ])

let error_page msg =
  Html5.(html
           (head (title (pcdata "ERROR; Hitscore Web")) [])
           (body [
             p [pcdata (sprintf "Histcore's error web page: %s"
                          Time.Ofday.(now () |> to_string))];
             p [ksprintf pcdata "Error: %s" msg];
           ]))

let default hsc =
  let html =
    flowcells hsc
    >>= fun html_flowcells ->
    return
      Html5.(html
               (head (title (pcdata "Hitscore Web")) [])
               (body [
                 html_flowcells;
                 p [pcdata (sprintf "Histcore's default web page: %s"
                              Time.Ofday.(now () |> to_string))]
               ])) in
  Lwt.bind html (function
  | Ok html ->
    Lwt.return html
  | Error (`pg_exn e) ->
    Lwt.return (error_page (sprintf "PGOCaml: %s" (Exn.to_string e)))
  | Error (`layout_inconsistency (_, _)) ->
    Lwt.return (error_page 
                  "Layout Inconsistency: Complain at bio.gencore@bio.nyu.edu")
  )



let () =

  let hitscore_configuration = Hitscore_lwt.configure () in

  Eliom_services.register_eliom_module
    "hitscoreweb" 
    (fun () ->
      (Eliom_output.Html5.register ~service:Services.default (fun () () ->
        default hitscore_configuration))
    )

