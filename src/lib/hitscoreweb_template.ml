open Hitscoreweb_std

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication


let html_of_error = 
  let open Html5 in
  function
  | `eliom_404 -> [pcdataf "Error 404."]
  | `eliom_wrong_parameter -> [pcdataf "Error 404 (wrong parameter)."]
  | `eliom_typing_error _ -> [pcdataf "Error 404 (wrong parameter types)."]
  | `io_exn e -> [pcdataf "Generic I/O exception: %s" (Exn.to_string e)]
  | `auth_state_exn e ->
    [pcdataf "Authentication-state exception: %s" (Exn.to_string e)]
  | `pg_exn e -> [pcdataf "PGOCaml exception: %s" (Exn.to_string e)]
  | `no_person_with_that_email email ->
    [pcdataf "There is no person with that email: ";
     code [pcdata email];
     pcdata "."]
  | `no_flowcell_named name ->
    [pcdata "There is no flowcell with that serial name: ";
     code [pcdata name];
     pcdata "."]
  | `sample_sheet_kind_not_found i ->
    [pcdataf "Cannot find this sample-sheet: %ld." i]
  | `sample_sheet_should_a_lonely_file p ->
    [pcdataf "This sample-sheet should be lonely in its volume: %ld."
        p.Layout.Record_sample_sheet.id]
  | `cannot_recognize_file_type s ->
    [pcdataf "The file-system is in a bad state, cannot recognize this \
              file-type: %s." s]
  | `inconsistency_inode_not_found inode ->
    [pcdataf "The file-system is in a bad state, cannot find file: %ld." inode]
  | `sample_sheet_kind_of_string s ->
    [pcdataf "Could not transform %S to a sample-sheet kind." s;]
  | `nothing_to_edit (types, values) ->
    [pcdataf "There is nothing to edit.";]
  | `not_implemented action ->
    [pcdataf "The action %S is not implemented … yet." action;]
  | `unknown_layout_action s ->
    [pcdataf "Cannot understand this action: %S." s;]
  | `did_not_get_one_row (name, not_one_row) ->
    [pcdataf "Getting a value did not return right: %s (%d rows)." 
        name (List.length not_one_row);]
  | `wrong_layout_typing name ->
    [pcdataf "The record %S is not well typed w.r.t the official Layout." name;]
  | `layout_edit_coservice_error e ->
    [pcdata "Error while editing: ";
     match e with
     | `fields_wrong_typing -> pcdata "Fields have wrong types"
     | `wrong_id -> pcdata "Could not get a decent g_id value"
     | `wrong_rights -> pcdata "You don't have enough access rights to do edit this.."
     | `io_exn e -> pcdataf "I/O Error: %s" (Exn.to_string e)
     | `layout_inconsistency (`record_log, _) -> 
       pcdata "Error while logging (chances are that the editing actually worked!)"
     | `pg_exn e -> pcdataf "PostgreSQL Error: %s" (Exn.to_string e)
    ]
  | `layout_inconsistency (place, problem) ->
    let place_presentation =
      let r = pcdata "the record " in
      let f = pcdata "the function " in
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
      | `record_sample_sheet ->   [ r; code [pcdata "record_sample_sheet"]]
      | `file_system ->   [ r; code [pcdata "file_system"]]
      | `function_bcl_to_fastq -> [ f; code [pcdata "bcl_to_fastq"]]
      | `function_assemble_sample_sheet      -> [f; codef "assemble_sample_sheet "]
      | `function_delete_intensities         -> [f; codef "delete_intensities "]
      | `function_dircmp_raw                 -> [f; codef "dircmp_raw "]
      | `function_prepare_unaligned_delivery -> [f; codef "prepare_unaligned_delivery"]
      | `function_transfer_hisqeq_raw        -> [f; codef "transfer_hisqeq_raw "]
      | `record_agarose_gel                  -> [r; codef "agarose_gel "]
      | `record_bcl_to_fastq_unaligned       -> [r; codef "bcl_to_fastq_unaligned"]
      | `record_bioanalyzer                  -> [r; codef "bioanalyzer "]
      | `record_client_fastqs_dir            -> [r; codef "client_fastqs_dir "]
      | `record_hiseq_checksum               -> [r; codef "hiseq_checksum "]
      | `record_inaccessible_hiseq_raw       -> [r; codef "inaccessible_hiseq_raw"]
      | `record_invoicing                    -> [r; codef "invoicing "]
      | `record_key_value                    -> [r; codef "key_value "]
      | `record_log                          -> [r; codef "log "]
      | `record_protocol                     -> [r; codef "protocol "]
    in
    let error_message =
      match problem with
      | `select_did_not_return_one_tuple (s, i) ->
        [code [ksprintf pcdata
                  "(select_did_not_return_one_tuple %s %d)" s i]]
      | `more_than_one_person_with_that_email ->
        [pcdata "There is (are?) more than one person with that email address."]
      | `more_than_one_flowcell_called s ->
        [ksprintf pcdata "There are more than one flowcells called %s" s]
      | `insert_did_not_return_one_id (s, l) ->
        [pcdataf "Insert in %s did not return one id but %d." s (List.length l)]
    in
    ([pcdata "Layout Inconsistency in "]
     @ place_presentation
     @ [pcdata ":"; br ()]
     @ error_message)



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
      Option.value_map title ~default:"Gencore" ~f:(sprintf "Gencore: %s")
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
      h1 [ksprintf pcdata "Gencore: Error Page"];
      p [ksprintf pcdata "An error occurred on %s:"
            Time.(now () |> to_string)];
      div msg;
      p [pcdata "Please complain at ";
         codef "bio.gencore@nyu.edu";
         pcdataf "."];]
    |! Lwt.return
  in
  Lwt.bind html_result (function
  | Ok html -> Lwt.return html
  | Error e -> error_page (html_of_error e))

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
      h1 [ksprintf pcdata "Gencore: %s" main_title];
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
