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
      let lane = ref 0 in
      Layout.Record_flowcell.(
        cache_value ~dbh one >>| get_fields
        >>= fun {serial_name; lanes} ->
        of_list_sequential (Array.to_list lanes) (fun lane_t ->
          incr lane;
          Layout.Record_lane.(
            cache_value ~dbh lane_t >>| get_fields
            >>= fun {
  	      seeding_concentration_pM ;
  	      total_volume ;
  	      libraries ;
  	      pooled_percentages ;
  	      requested_read_length_1 ;
  	      requested_read_length_2 ;
  	      contacts ; } ->
            of_list_sequential (Array.to_list contacts) (fun person_t ->
              Layout.Record_person.(
                cache_value ~dbh person_t >>| get_fields
                >>= fun { given_name; family_name; _ } ->
                return (given_name, family_name)))
            >>= fun people ->
            of_list_sequential
              (Array.to_list (Array.mapi libraries ~f:(fun i a -> (i,a))))
              (fun (i, ilibt) ->
                Layout.Record_input_library.(
                  cache_value ~dbh ilibt >>| get_fields
                  >>= fun { library; _ } ->
                  Layout.Record_stock_library.(
                    cache_value ~dbh library >>| get_fields
                    >>= fun { name; project; _ } ->
                    return (name, project))))
            >>= fun libs ->
            return Html5.(
              let na = pcdata "—" in
              let cellt =
                td  ~a:[ a_style "border: 1px  solid grey; padding: 2px; \
                                  max-width: 40em;" ] in
              let cellf =
                td  ~a:[ a_style "border: 1px  solid grey; padding: 4px; \
                                  text-align: right;" ] in
              let opt o f = Option.value_map ~default:na o ~f in
              let pcf = (ksprintf pcdata "%.0f") in
              tr [
                cellt [ksprintf pcdata "Lane %d" !lane];
                cellf [opt seeding_concentration_pM pcf];
		cellf [opt total_volume pcf];
                cellt (List.map people 
                        (fun (f,l) -> [ ksprintf pcdata "%s %s" f l; br () ])
                                      |! List.flatten);
                cellt [pcdata 
                          (List.map libs 
                             (function
                             | (l, None) -> sprintf "%s" l
                             | (l, Some p) -> sprintf "%s.%s" p l)
                                            |! String.concat ~sep:", ")];
              ]))))
    in
    lanes >>= fun lanes ->
    return Html5.(div [
      let head_cell =  th ~a:[ a_style "border: 1px  solid black" ] in
      table
        ~a:[ a_style "border: 3px  solid black; \
                      border-collapse: collapse; " ]
        (* ~caption:(caption [pcdata "bouh"]) *)
        (* ~columns:[colgroup [col (); col ()]] *)
        (tr [ head_cell [pcdata "Lane Nb"]; 
              head_cell [pcdata "Seeding C."];
              head_cell [pcdata "Vol."];
              head_cell [pcdata "Contacts"];
              head_cell [pcdata "Libraries"];
            ])
        lanes
    ]
    )
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

