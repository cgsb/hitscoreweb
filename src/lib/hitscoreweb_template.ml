open Hitscoreweb_std

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

let default ?(title) content =
  let page page_title auth_state html_stuff =
    Html5.(
      html
        (head (title (pcdata page_title)) [])
        (body [
          div [
            Services.(link default) [pcdata "Home"] ();
            div auth_state;
          ];
          hr ();
          div html_stuff
        ]))
  in
  let html_result =
    let page_title = 
      Option.value_map title ~default:"Hitscoreweb" ~f:(sprintf "HSW: %s")
    in
    Authentication.display_state ()
    >>= fun auth_state ->
    content
    >>= fun good_content ->
    return (page page_title auth_state good_content)
  in
  let open Html5 in
  let error_page msg =
    page "Error" [] [
      h1 [ksprintf pcdata "Histcore's Error Page"];
      p [ksprintf pcdata "An error occurred on %s:"
            Time.(now () |> to_string)];
      div msg;
      p [pcdata "Please complain at bio.gencore@nyu.edu."];] in
  Lwt.bind html_result (function
  | Ok html ->
    Lwt.return html
  | Error (`io_exn e) ->
    Lwt.return (error_page [
      ksprintf pcdata "Generic I/O exception: %s" (Exn.to_string e)])
  | Error (`auth_state_exn e) ->
    Lwt.return (error_page [
      ksprintf pcdata "Authentication-state exception: %s" (Exn.to_string e)])
  | Error (`pg_exn e) ->
    Lwt.return (error_page [
      ksprintf pcdata "PGOCaml exception: %s" (Exn.to_string e)])
  | Error (`no_person_with_that_email email)->
    Lwt.return (error_page [
      ksprintf pcdata "There is no person with that email: ";
      code [pcdata email];
      pcdata "."])
  | Error (`no_flowcell_named name) ->
    Lwt.return (error_page [
      ksprintf pcdata "There is no flowcell with that serial name: ";
      code [pcdata name];
      pcdata "."])
  | Error (`layout_inconsistency (place, problem)) ->
    let place_presentation =
      let r = pcdata "the record " in
      match place with
      | `record_person ->        [ r; code [pcdata "person"        ]]  
      | `record_organism ->      [ r; code [pcdata "organism"      ]]  
      | `record_sample ->        [ r; code [pcdata "sample"        ]]  
      | `record_stock_library -> [ r; code [pcdata "stock_library" ]]
      | `record_flowcell      -> [ r; code [pcdata "record_flowcell"      ]] 
      | `record_input_library -> [ r; code [pcdata "record_input_library" ]] 
      | `record_lane          -> [ r; code [pcdata "record_lane"          ]]
      | `record_hiseq_raw     -> [ r; code [pcdata "record_hiseq_raw"     ]]
      | `record_custom_barcode -> [ r; code [pcdata "record_custom_barcode"]]
    in
    let error_message =
      match problem with
      | `select_did_not_return_one_cache (s, i) ->
        [code [ksprintf pcdata
                  "(select_did_not_return_one_cache %s %d)" s i]]
      | `more_than_one_person_with_that_email ->
        [pcdata "There is (are?) more than one person with that email address."]
      | `more_than_one_flowcell_called s ->
        [ksprintf pcdata "There are more than one flowcells called %s" s]
    in
    Lwt.return (error_page (
      [ksprintf pcdata "Layout Inconsistency in "]
      @ place_presentation
      @ [pcdata ":"; br ()]
                  @ error_message
    )))



