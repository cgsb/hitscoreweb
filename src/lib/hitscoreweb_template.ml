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


module Display_service = struct


  type table_cell_html5 = HTML5_types.flow5 Html5.elt list

  type content =
  | Description of
      (HTML5_types.phrasing Html5.elt * HTML5_types.flow5 Html5.elt) list 
  | Section of HTML5_types.phrasing Html5.elt * content 
  | List of content list
  | Table of [`head of table_cell_html5
             |`text of table_cell_html5
             |`number of table_cell_html5 ] list list
  | Paragraph of HTML5_types.flow5 Html5.elt list

  let description l = Description l
  let description_opt l = Description (List.filter_opt l)
  let content_section t c = Section (t, c)
  let content_list l = List l
  let content_table ?(transpose=false) l =
    let t = function
      | [] -> []
      | hl :: tll ->
        (* let lgth = List.length hl in *)
        List.mapi hl (fun i h ->
          h :: (List.map tll (fun tl ->
            Option.value ~default:(`text []) (List.nth tl i))))
    in
    Table (if transpose then t l else l)
        
  let paragraph l = Paragraph l

  let rec html_of_content ?(section_level=2) content =
    let open Html5 in
    let h = function
      | 2 -> h2
      | 3 -> h3
      | 4 -> h4
      | 5 -> h5
      | 6 -> h6
      | _ -> span in
    match content with
    | Paragraph l -> div l
    | Description desc ->
      ul (List.map desc (fun (l, r) ->
        li [strong [l]; pcdata ": "; r; pcdata "."]));
    | Section (title, content) ->
      div [h section_level [title];
           html_of_content ~section_level:(section_level + 1) content]
    | List cl ->
      div (List.map cl (html_of_content ~section_level))
    | Table [] -> div []
    | Table (h :: t) ->
      let make_cell = function
        | `head c -> 
          td ~a:[ a_style "border: 1px solid black; padding: 2px; color: red" ] c
        | `text c ->
          td  ~a:[ a_style "border: 1px  solid grey; padding: 2px; \
                            max-width: 40em;" ] c
        | `number c ->
          td  ~a:[ a_style "border: 1px  solid grey; padding: 4px; \
                            text-align: right;" ] c
      in
      div [
        table
          ~a:[ a_style "border: 3px  solid black; \
                        border-collapse: collapse; " ]
        (* ~caption:(caption [pcdata "bouh"]) *)
        (* ~columns:[colgroup [col (); col ()]] *)
          (tr (List.map h make_cell))
          (List.map t (fun l -> 
            tr (List.map l make_cell)))
      ]

  let make_content ~hsc ~main_title content =
    let open Html5 in
    content >>= fun content ->
    return [
      h1 [ksprintf pcdata "Hitscoreweb: %s" main_title];
      html_of_content content]

  let make ~hsc ~main_title content =
    let html_content = make_content ~hsc ~main_title content in
    default ~title:main_title html_content

end


module Authentication_error = struct

  let make_content ~hsc ~main_title content =
    let open Html5 in
    content >>= fun content ->
    return [
      h1 [pcdataf "Authentication Error: %s" main_title];
      div [
          div content;
        pcdata "Perhaps should you ";
        Services.(link login) [pcdata "login"] ();
        pcdataf "? or maybe request more access rights?"
      ];
    ]

end
