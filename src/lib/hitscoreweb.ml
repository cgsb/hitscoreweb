open Hitscoreweb_std



module Services = struct

  let default =
    Eliom_services.service
      ~path:[""] ~get_params:Eliom_parameters.unit ()

  let flowcells =
    Eliom_services.service
      ~path:["flowcells"] ~get_params:Eliom_parameters.unit ()

  let flowcell =
    Eliom_services.service
      ~path:["flowcell"] ~get_params:Eliom_parameters.(string "serial") ()
end


let error_page msg =
  Html5.(html
           (head (title (pcdata "ERROR; Hitscore Web")) [])
           (body [
             p [pcdata (sprintf "Histcore's error web page: %s"
                          Time.Ofday.(now () |> to_string))];
             p [ksprintf pcdata "Error: %s" msg];
           ]))

let flowcells hsc =
  Hitscore_lwt.db_connect hsc
  >>= fun dbh ->
  Layout.Record_flowcell.(
    get_all ~dbh
    >>= fun flowcells ->
    of_list_sequential flowcells ~f:(fun f ->
      cache_value ~dbh f >>| get_fields
      >>= fun {serial_name; lanes} ->
      return Html5.(li [
        Eliom_output.Html5.a Services.flowcell [pcdata serial_name] serial_name;
        pcdata "."
      ]))
    >>= fun ul ->
    return (List.length flowcells, ul)
  )
  >>= fun (length, items) ->
  return Html5.(div [
    h1 [ ksprintf pcdata "%d Flowcells" length];
    ul items
  ])

let one_flowcell hsc serial_name =
  Hitscore_lwt.db_connect hsc
  >>= fun dbh ->
  Layout.Search.record_flowcell_by_serial_name ~dbh serial_name
  >>= function
  | [ one ] ->
    let lanes =
      Layout.Record_flowcell.(
        cache_value ~dbh one >>| get_fields
        >>= fun {serial_name; lanes} ->
        of_list_sequential (Array.to_list lanes) (fun lane_t ->
          Layout.Record_lane.(
            cache_value ~dbh lane_t >>| get_fields
            >>= fun {seeding_concentration_pM; _} ->
            ksprintf return "seeding_concentration_pM: %f"
              (Option.value ~default:99999. seeding_concentration_pM)
          )))
    in
    lanes >>= fun lanes ->
    return Html5.(ul (List.mapi lanes (fun i s -> 
      li [ksprintf pcdata "Lane %d: %s" i s])))
  | more ->
    error (`layout_inconsistency (`record_flowcell, 
                                  `more_than_one_flowcell_called serial_name))


let flowcells_service hsc =
  let html =
    flowcells hsc
    >>= fun html_flowcells ->
    return
      Html5.(html
               (head (title (pcdata "Hitscore Web")) [])
               (body [
                 html_flowcells;
               ])) in
  Lwt.bind html (function
  | Ok html ->
    Lwt.return html
  | Error (`pg_exn e) ->
    Lwt.return (error_page (sprintf "PGOCaml: %s" (Exn.to_string e)))
  | Error (`layout_inconsistency (_, _)) ->
    Lwt.return (error_page 
                  "Layout Inconsistency: Complain at bio.gencore@nyu.edu")
  )

let default_service hsc =
  Lwt.return Html5.(
    html
      (head (title (pcdata "Hitscoreweb: Default")) [])
      (body [
        h1 [pcdata "Services:"];
        ul [
          li [
            Eliom_output.Html5.a Services.flowcells [pcdata "Flowcells"] ()
          ];
        ];
        p [pcdata (sprintf "Histcore's default web page: %s"
                     Time.(now () |> to_string))];
      ]))

let flowcell_service hsc serial_name =
  let html =
    one_flowcell hsc serial_name
    >>= fun html_flowcell ->
    return Html5.(
      html
        (head (title (ksprintf pcdata "Hitscoreweb: Flowcell %s" serial_name)) [])
        (body [
          h1 [ksprintf pcdata "Hitscoreweb: Flowcell %s" serial_name];
          html_flowcell
        ]))
  in
  Lwt.bind html (function
  | Ok html ->
    Lwt.return html
  | Error (`pg_exn e) ->
    Lwt.return (error_page (sprintf "PGOCaml: %s" (Exn.to_string e)))
  | Error (`layout_inconsistency (_, _)) ->
    Lwt.return (error_page 
                  "Layout Inconsistency: Complain at bio.gencore@nyu.edu")
  )


let () =

  let hitscore_configuration = Hitscore_lwt.configure () in

  Eliom_services.register_eliom_module
    "hitscoreweb" 
    (fun () ->
      Eliom_output.Html5.register ~service:Services.default (fun () () ->
        default_service hitscore_configuration);
      Eliom_output.Html5.register ~service:Services.flowcells (fun () () ->
        flowcells_service hitscore_configuration);
      Eliom_output.Html5.register ~service:Services.flowcell (fun (serial) () ->
        flowcell_service hitscore_configuration serial);
    )

