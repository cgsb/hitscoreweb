{shared{
open Hitscoreweb_std
module Services = Hitscoreweb_services
}}
module Authentication = Hitscoreweb_authentication

let css_triangle_arrow ~css_class ?(height_px=10) ?(background="#ddd")
    ?(margin="2px") ?(color="#333") (direction : [ `up | `down ]) =
  let buf = Buffer.create 42 in
  let out fmt = ksprintf (fun s -> (Buffer.add_string buf s)) fmt in
  out ".%s {\n" css_class;
  out " height: %dpx; width: %dpx;" height_px (height_px * 2);
  out "background-color: %s;\n" background;
  out "position: relative; float: left; margin: %s;\n}\n" margin;
  out ".%s:after {\n" css_class;
  out " content: ' '; height: 0; position: absolute; width: 0;\n";
  out " border: %dpx solid transparent;" height_px;
  out "border-%s-color: %s;\n"
    (match direction with `up -> "top" | `down -> "bottom") color;
  out " top: %dpx; left: 0px;}\n"
    (match direction with `up -> 0 | `down -> - height_px);
  Buffer.contents buf

let css_service_handler ~configuration () () =
  let open Lwt in
  let css = Buffer.create 42 in
  let out fmt = ksprintf (fun s -> (Buffer.add_string css s)) fmt in
  let side_margins = "5%" in
  out "body { font-family: sans-serif; margin-left: %s; margin-right: %s; }"
    side_margins side_margins;

  out "%s" (css_triangle_arrow ~css_class:"sort_normal_button" `up);
  out "%s" (css_triangle_arrow ~css_class:"sort_reverse_button" `down);
  
  Lwt.return (Buffer.contents css)

let html_of_error = 
  let open Html5 in
  function
  | `non_https_login -> [pcdata "Login service not on HTTPS: FORBIDDEN"]
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
  | `raw_data_path_not_configured ->
    [pcdataf "The path to the raw data has not been configured."]
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
      | `no_last_modified_timestamp _ ->
        [pcdataf "Missing last-modified information."]
    in
    ([pcdata "Layout Inconsistency in "]
     @ place_presentation
     @ [pcdata ":"; br ()]
     @ error_message)

let default ?(title) content =
  let page page_title auth_state html_stuff =
    Html5.(
      let debug_bloc =
        match !debug_messages with
        | [] -> div []
        | l -> 
          div [
            hr ();
            pcdataf "Debug Messages:";
            br ();
            ul (List.map l (fun (t, m) ->
              li [pcdataf "[%s]: " (Time.to_string t);
                  codef "%s" m]));
          ] in
      html
        (head (title (pcdata page_title)) [
          link ~rel:[`Stylesheet] ~href:(uri_of_string "hitscoreweb.css") ();
          link ~rel:[`Stylesheet] ~href:(Eliom_output.Html5.make_uri
                                           ~service:Services.(stylesheet ()) ()) ();
        ])
        (body [
          div [
            Services.(link default) [pcdata "Home"] ();
            div auth_state;
          ];
          hr ();
          div html_stuff;
          debug_bloc;
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
  | Ok html -> 
    Lwt.return html
  | Error e -> error_page (html_of_error e))

type table_cell_html5 = HTML5_types.flow5 Html5.elt list
  
type content =
| Description of
    (HTML5_types.phrasing Html5.elt * HTML5_types.flow5 Html5.elt) list 
| Section of HTML5_types.phrasing Html5.elt * content 
| List of content list
| Table of [`head of table_cell_html5
           |`text of table_cell_html5
           |`sortable of string * table_cell_html5
           |`number of table_cell_html5 ] list list
| Paragraph of HTML5_types.flow5 Html5.elt list
    
let content_description l = Description l
let content_description_opt l = Description (List.filter_opt l)
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
    
let content_paragraph l = Paragraph l

let _global_table_ids = ref 0

let td_on_click_to_sort do_something order cell_id idx id =
  let order_multiplier =
    match order with `normal -> 1 | `reverse -> -1 in
  {{
    if %do_something then (
      let tab =
        Js.coerce_opt 
          (Dom_html.document##getElementById (Js.string %id))
          Dom_html.CoerceTo.table (fun _ -> assert false) in
      let rows = tab##rows in
      let get_cell_title r c = 
        Js.Optdef.(
          let (>>=) = bind in
          let opt =
            rows##item(r)
            >>= fun row ->
            row##cells##item(c)
            >>= fun cell ->
            return (Js.to_string cell##title)
          in
          get opt (fun () -> "")
        ) in
      let array = Array.create (rows##length - 1) ("", Js.undefined) in
      for i = 1 to rows##length - 1 do
        array.(i - 1) <- (get_cell_title i %idx, rows##item(i));
      done;
      Array.stable_sort (fun (x,_) (y,_) ->
        %order_multiplier *
          (try compare (int_of_string x) (int_of_string y)
           with e ->
             String.compare x y)) array;
      for i = 1 to rows##length - 1 do
        Js.Optdef.iter (snd array.(i - 1)) (fun row ->
          tab##deleteRow(i);
          tab##insertRow(i)##innerHTML <- row##innerHTML);
      done;
    )   
}}

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
    let make_cell ?orderable idx cell =
      let really_orderable =
        (* Really orderable: if there is some sortable element in that column. *)
        List.exists (List.map t (fun l -> List.nth l idx))
          ~f:(function Some (`sortable _) -> true | _ -> false) in
      let cell_id = incr _global_table_ids; sprintf "cell%d" !_global_table_ids in
      let buttons =
        Option.value_map (if really_orderable then orderable else None)
          ~default:[]
          ~f:(fun tableid ->
            let td_onclick order =
              td_on_click_to_sort true order cell_id idx tableid in
            [
              div ~a:[
                a_title "sort:normal";
                a_class ["sort_normal_button"];
                a_onclick (td_onclick `normal);]
                [];
              div ~a:[
                a_title "sort:reverse";
                a_class ["sort_reverse_button"];
                a_onclick (td_onclick `reverse);]
                []; ])
      in
      match cell with
      | `head (c) -> 
        td ~a:[
          a_id cell_id;
          a_style "border: 1px solid black; padding: 2px; color: red" ] 
          ([div c] @ buttons)
      | `sortable (title, cell) ->
        td  ~a:[ a_title title;
                 a_style "border: 1px  solid grey; padding: 2px; \
                            max-width: 40em;" ] cell
      | `text cell ->
        td  ~a:[
          a_style "border: 1px  solid grey; padding: 2px; \
                            max-width: 40em;" ] cell
      | `number c ->
        td  ~a:[
          a_style "border: 1px  solid grey; padding: 4px; \
                            text-align: right;" ] c
    in
    let id = incr _global_table_ids; sprintf "table%d" !_global_table_ids in
    div [
      table 
        ~a:[ a_id id;
             a_style "border: 3px  solid black; \
                        border-collapse: collapse; " ]
        (tr (List.mapi h (make_cell ~orderable:id)))
        (List.map t (fun l -> tr (List.mapi l (make_cell ?orderable:None))))
    ]

let make_content ~configuration ~main_title content =
  let open Html5 in
  content >>= fun content ->
  return [
    h1 [ksprintf pcdata "Gencore: %s" main_title];
    html_of_content content]
    
let make_authentication_error ~configuration ~main_title content =
  let open Html5 in
  content >>= fun content ->
  return [
    h1 [pcdataf "Authentication Error: %s" main_title];
    div [
      div content;
      pcdata "Perhaps should you login? or maybe request more access rights?"
    ];
  ]

