{shared{
open Hitscoreweb_std
module Services = Hitscoreweb_services
}}
module Authentication = Hitscoreweb_authentication

module Msg = Hitscoreweb_messages

let make_unsafe_eval_string_onload to_run =
  ignore {unit{
    Eliom_lib.debug "Running %S" %to_run;
    Js.Unsafe.eval_string %to_run
  }}

let css_triangle_arrow ~css_class ?(height_px=10) ?(background="#ddd")
    ?(margin="2px") ?(color="#333") (direction : [ `up | `down ]) =
  let buf = Buffer.create 42 in
  let out fmt = ksprintf (fun s -> (Buffer.add_string buf s)) fmt in
  out ".%s {\n" css_class;
  out " height: %dpx; width: %dpx;" height_px (height_px * 2);
  out "background-color: %s;\n" background;
  out "position: relative; float: right; margin: %s;\n}\n" margin;
  out ".%s:after {\n" css_class;
  out " content: ' '; height: 0; position: absolute; width: 0;\n";
  out " border: %dpx solid transparent;" height_px;
  out "border-%s-color: %s;\n"
    (match direction with `up -> "top" | `down -> "bottom") color;
  out " top: %dpx; left: 0px;}\n"
    (match direction with `up -> 0 | `down -> - height_px);
  Buffer.contents buf

let color_theme =
object
  method title_violet = "#30053F"
  method light_violet = "#ECD5F4"
  method nyu_violet = "#634283"
end

let _local_styles = ref []

module Local_style = struct
  type element_of_style = [
  | `css_class of string * string list
  ]
  type t = element_of_style list ref
  let create () : t = ref []
  let add_class (t: t) name style =
    t := `css_class (name, style) :: !t;
    name
  let use (t: t) = _local_styles := !t :: !_local_styles
end
{client{
module Local_style = struct
  let create () = ()
  let add_class _ name _ = name
  let use _ = ()
end
}}

let css_service_handler ~configuration () () =
  let open Lwt in
  let css = Buffer.create 42 in
  let out fmt = ksprintf (fun s -> (Buffer.add_string css s)) fmt in
  let light_grey = "#eee" in

  let () =
    let background = light_grey in
    out "%s" (css_triangle_arrow ~background ~css_class:"sort_normal_button" `up);
    out "%s" (css_triangle_arrow ~background ~css_class:"sort_reverse_button" `down);
  in

  out "html, body {  height: 100%% }"; (* this required by the "footer hack" *)
  out "body {font:13px Helvetica,arial,freesans,clean,sans-serif;
             line-height:1.4;padding:0px; margin:0px;width:100%%;
             }";

  let top_baner_max_height, main_margin_top = "5em", "7em" in

  out ".top_banner,.footer { \
    right: 0%%; left: 0%%; width: 100%%; color: white;
    background-color: %s; background-opacity: 1;
    }" color_theme#nyu_violet;
  out ".top_banner {position: fixed; top:0px; \
                     z-index: 100; \
                    padding: 4px; margin: 0px;  \
                     }" ;
  out ".top_banner { max-height: %s ; }" top_baner_max_height;
  out ".top_banner .top_menu a {
                    text-decoration: none;
                    padding: 4px; margin-left: 10px;
                    /* border-radius: 5px;
                    border:1px solid #421857;
                    border-bottom:2px solid #7F1DAF;
                    border-top:1px solid #421857; */
        }\n";
  out ".top_banner .top_menu { margin-bottom: 10px; }\n";
  out ".top_banner a:link {color : white; }\n";
  out ".footer a:link {color : white; }\n";
  out ".top_banner a:hover {background-color : %s; }\n" color_theme#title_violet;
  out ".footer a:hover {background-color : %s; }\n" color_theme#title_violet;
  out ".top_banner a:visited {color : white;}\n";
  out ".footer a:visited {color : white;}\n";
  out ".top_banner form { display: inline; padding: 10px; }";
  out ".top_banner .main_menu { padding: 0px; display: inline; }\n";
  out ".wrapper { min-height: 100%%; }";
  out ".footer { position: relative; margin-bottom: 0; \
                 padding: 0px; margin-top: -1.8em; height: 1.8em; }";
  out ".main_page { padding-top: %s; width:98%%; padding-bottom: 3em; \
                    margin-left: 4px; margin-right: 4px; }"
    main_margin_top;
  out ".main_page a:link {color : #960F00; }\n";
  out ".main_page .like_link {color : #960F00; text-decoration: underline; }\n";
  out ".main_page a:hover {background-color : %s; }\n" color_theme#light_violet;
  out ".main_page a:visited {color : #480007; }\n";
  out ".main_page h1 {font-weight: 900; color : %s;
           font-variant: small-caps; font-size: 200%% }\n"
    color_theme#title_violet;
  out ".main_page h2 {font-weight: 900; color : %s;
                      font-size: 150%% }\n" color_theme#title_violet;
  List.iter [1, 230; 2, 210; 3, 190; 4, 170; 5, 150; 6, 130] (fun (h, s) ->
    out ".main_page h%d {font-size: %d%%}\n" h s
  );
  out ".content_table_head { color: #960F00; font-size: 110%%;
          background-color: %s; }\n" light_grey;
  out ".content_table_head,.content_table_text,.content_table_number {
          max-width: 40em;
          border: 1px solid black; padding: 3px; }";
  out ".content_table_number {text-align:right; font-family: monospace}\n";
  out ".odd_colored_row { background-color: %s }" color_theme#light_violet;
  out ".big_warning { background-color: #E03007; color: #D7E007; \
                      font-weight: bold; padding: 3px; font-size: 110%% }";

  List.iter !_local_styles (fun ls ->
    let open Local_style in
    List.iter ls (function
    | `css_class (name, style) ->
      out ".%s { %s }" name (String.concat ~sep:";" style)
    );
  );
  out "
    @media (max-width: 63em) {
      .doc_toc {
        margin-right: 1em;
      }
    }
    @media (min-width: 63em) {
      .doc_toc {
        position:fixed;
        overflow: auto; max-width: 40em;
        top: 100px; left: 53em;
        margin-right: 1em;
      }
    } ";
  out ".doc_doc {text-align: justify; max-width:50em;
         margin-right: 1em; margin-bottom: 3em;}";

  Lwt.return (Buffer.contents css)

let error_span content =
  let open Html5 in
  span ~a:[ a_style "background-color: #ecc" ] [strong content]

let error_box for_the_people fmt_for_the_priviledged =
  let open Html5 in
  ksprintf (fun for_the_priviledged ->
    logf "ERROR-BOX:\nUser: %s\nDetails: %s" for_the_people for_the_priviledged
    >>= fun () ->
    Authentication.authorizes (`view `error_details)
    >>= begin function
    | true ->
      let span =
        error_span [pcdataf "ERROR: %s: %s" for_the_people for_the_priviledged]
      in
      return span
    | false -> return (error_span [pcdata for_the_people])
    end
  ) fmt_for_the_priviledged

let string_of_backend_error e =
  begin match e with
  | `exn e
  | `connection e
  | `disconnection e
  | `query  (_, e) ->
    sprintf "DB BACKEND ERROR: %s" (Exn.to_string e)
  end

let string_of_layout_error = function
  | `db_backend_error (`query (q, e)) ->
    sprintf "Query %S failed: %s" q (Exn.to_string e)
  | `db_backend_error e ->
    string_of_backend_error e
  | `parse_evaluation_error (sol, e)
  | `parse_value_error (sol, e)
  | `parse_volume_error (sol, e) ->
    sprintf "error while parsing result [%s]: %s"
      (String.concat ~sep:", "
         (List.map sol (Option.value ~default:"NONE"))) (Exn.to_string e)
  | `parse_timestamp s ->
    sprintf "error while parsing %S as a timestamp!" s
  | `parse_sexp_error (sexp, e) ->
    sprintf "S-Exp parsing error: %S: %s"
      (Sexp.to_string_hum sexp) (Exn.to_string e)
  | `result_not_unique soll ->
    sprintf "result_not_unique: [%s]"
      (String.concat ~sep:", "
         (List.map soll (fun sol ->
           String.concat ~sep:", "
             (List.map sol (Option.value ~default:"NONE")))))
  | `wrong_add_value ->
    sprintf "WRONG-ADD-VALUE"
  | `wrong_version (v1, v2) ->
    sprintf "Wrong version: %s Vs %s" v1 v2



let string_of_error_sublevel poly_error =
  let open Html5 in
  String.concat ~sep:""
  (match poly_error with
  | `eliom_404 -> [sprintf "Error 404."]
  | `eliom_wrong_parameter -> [sprintf "Error 404 (wrong parameter)."]
  | `eliom_typing_error _ -> [sprintf "Error 404 (wrong parameter types)."]
  | `io_exn e -> [sprintf "Generic I/O exception: %s" (Exn.to_string e)]
  | `string s -> [s]
  | `auth_state_exn e ->
    [sprintf "Authentication-state exception: %s" (Exn.to_string e)]
  | `pg_exn e -> [sprintf "PGOCaml exception: %s" (Exn.to_string e)]
  | `person_not_found id ->
    [sprintf "There is no person with that identifier: %s" id]
  | `wrong_rights ->
    [sprintf "You don't have enough access rights to do edit this."]
  | `read_file_timeout (f, t) ->
    [sprintf "Error while reading file: "; sprintf "%s" f;
     sprintf ": timeout %f" t]
  | `read_file_error (f, e) ->
    [sprintf "Error while reading file: "; sprintf "%s" f;
     sprintf ": %s" (Exn.to_string e)]
  | `non_emptiness_violation ->
    [sprintf "You're trying to violate non-emptiness constraints!"]
  | `broker_not_initialized -> [sprintf "The query broker has not been initialized"]
  | `hiseq_runs e ->
    [sprintf "Service /hiseq_runs: %s"
        (match e with
        | `no_logged_user -> "No user logged"
        | `cannot_retrieve_person_affairs person ->
          "Cannot retrieve current person's stuff")]
  | `person_not_unique id ->
    [sprintf "There are too many persons with that identifier: %s." id]
  | `no_flowcell_named name ->
    [sprintf "There is no flowcell with that serial name: %s." name]
  | `sample_sheet_kind_not_found i ->
    [sprintf "Cannot find this sample-sheet: %d." i]
  | `sample_sheet_should_a_lonely_file p ->
    [sprintf "This sample-sheet should be lonely in its volume: %d."
        p.Layout.Record_sample_sheet.id]
  | `cannot_recognize_file_type s ->
    [sprintf "The file-system is in a bad state, cannot recognize this \
              file-type: %s." s]
  | `inconsistency_inode_not_found inode ->
    [sprintf "The file-system is in a bad state, cannot find file: %d." inode]
  | `sample_sheet_kind_of_string s ->
    [sprintf "Could not transform %S to a sample-sheet kind." s;]
  | `nothing_to_edit (types, values) ->
    [sprintf "There is nothing to edit.";]
  | `not_implemented action ->
    [sprintf "The action %S is not implemented … yet." action;]
  | `unknown_layout_action s ->
    [sprintf "Cannot understand this action: %S." s;]
  | `did_not_get_one_row (name, not_one_row) ->
    [sprintf "Getting a value did not return right: %s (%d rows)."
        name (List.length not_one_row);]
  | `wrong_layout_typing name ->
    [sprintf "The record %S is not well typed w.r.t the official Layout." name;]
  | `raw_data_path_not_configured ->
    [sprintf "The path to the raw data has not been configured."]
  | `bcl_to_fastq_succeeded_without_result b2f ->
    [sprintf "Layout problem: B2F succeeded without result"]
  | `root_directory_not_configured ->
    [sprintf "The root directory is not configured"]
  | `wrong_unaligned_volume _ ->
    [sprintf "B2F/Unaligned/ virtual volume does not have the expected structure"]
  | `wrong_fastx_volume _ ->
    [sprintf "FASTX/Unaligned/ virtual volume does not have the expected structure"]
  | `parse_flowcell_demux_summary_error e ->
    [sprintf "Error while parsing demux-summary: %s" (Exn.to_string e)]
  | `person_edit_coservice_error exn ->
    sprintf "Error while editing: " :: [sprintf "%s" (Exn.to_string exn)]
  | `no_lane_index (fcid, pointer) ->
    [sprintf "no_lane_index: %S (lane %d)" fcid pointer.Layout.Record_lane.id]
  | `xml_parsing_error ((l, c), e) ->
    [sprintf "Error while parsing the XML: Line %d, Character %d: %s"
        l c (Xml_tree.error_message e)]
  | `email_verification e ->
    [sprintf "Error while verifying email: %s"
        (match e with
        | `cannot_find_old_email old ->
          sprintf "old email not there: %S" old
        | `cannot_find_user_key (id, key) ->
          sprintf "cannot find find user %s with key %s" id key)]
  | `more_than_one_flowcell_called serial_name ->
    [sprintf "More than one flowcell is called: %s" serial_name]
  | `Layout (where, what) ->
  [sprintf "LAYOUT-ERROR (%s): %s"
      (match where with
      | `Dump -> "Dump"
      | `Identification -> "Identification"
      | `File_system -> "File-system"
      | `Function f -> sprintf  "function %S" f
      | `Record r -> sprintf "record %S" r)
      (string_of_layout_error what)
  ]
  | `db_backend_error _
  | `result_not_unique _
  | `parse_evaluation_error _
  | `parse_volume_error _
  | `parse_value_error _ as e->
    [sprintf "%s" (string_of_layout_error e)]
  | `broker_error _ -> []
  | `Layout_service _ -> []
  | `user_data _ -> []
  | `cannot_find_secondary_email
  | `email_verification_in_progress _
  | `sendmail _
  | `wrong_parameter _ -> [sprintf "some error with email verification"]
  | `json_parsing  e ->
    [sprintf "JSon parsing: "; Exn.(to_string e)]
  | `wrong_chunk _ ->
    [sprintf "Client-server communication error: wrong_chunck"]
  | `wrong_date s ->
    [sprintf "Cannot parse date: %s" s]
  | `wrong_form_returned s ->
    [sprintf "Wrong form returned: %s" s]
  | `wrong_credentials -> ["wrong_credentials"]
  | `system_command_error (s, e) ->
    [sprintf "System command error:\nCommand: %S\nError: %s" s
        (match e with
        | `exited i -> sprintf "Exited with %d" i
        | `exn e -> sprintf "Exn %s" Exn.(to_string e)
        | `signaled i -> sprintf "Got signal %d" i
        | `stopped i -> sprintf "Got stopped (%d)" i)]
  | `no_user_logged -> ["no_user_logged"]
  | `sexp_parsing_error e -> [sprintf "sexp_parsing_error: %s" Exn.(to_string e)]
  )

let string_of_error  poly_error =
  match poly_error with
  | `broker_error e -> string_of_error_sublevel e
  | `Layout_service e -> string_of_error_sublevel e
  | `user_data (s, e) ->
    sprintf "User-data: %s, %s" s (string_of_error_sublevel e)
  | e -> string_of_error_sublevel e

let a_link ?(a=[]) ?fragment service content args =
  let aa =  a in
  let open Html5 in
  let on_click = {{
    (* Without the on_click, firefox/chrome do not display the "in progress"
       turning thing ... *)
    fun ev -> ()
  }} in
  Html5.a ~a:(a_onclick on_click :: aa) ?fragment
    ~service:(service ()) content args

let menu_ul () =
  let open Html5 in
  let real_li s = return (Some (li ~a:[ a_class ["main_menu"]] s)) in
  let potential_li (cap, s) =
    Authentication.authorizes cap
    >>= function
    | true ->  real_li s
    | false -> return None
  in
  while_sequential ~f:potential_li [
    (`view `all_flowcells,
     [a_link Services. hiseq_runs [pcdata "HiSeq 2000 Runs"] ()]);
    (`view `persons,
     [a_link Services.persons [pcdata "Persons"] (None, [])]);
    (`view `libraries,
     [a_link Services.libraries [pcdata "Libraries"] ([`basic; `fastq], []); ]);
    (`view `facility_statistics,
     [a_link Services.facility_statistics [pcdata "Stats"] (); ]);
    (`view `all_evaluations,
     [a_link Services.evaluations [pcdata "Function evaluations"] ()]);
    (`view `layout,
     [a_link Services.layout [ pcdata "Layout Navigaditor" ] ([], [])]);
    (`view `log, [a_link Services.log [ pcdata "Log" ] ()]);
    (`view `test_service, [a_link Services.test [ pcdata "Test" ] ()]);
    (`view `all_uploads, [a_link Services.uploads [ pcdata "Uploads" ] ()]);
    (`view `submission_forms,
     [a_link Services.submission_forms [ pcdata "Submission forms" ] ()])
  ]
  >>= fun ul_opt ->
  match List.filter_opt ul_opt with
  | [] -> return None
  | items -> return (Some (ul ~a:[ a_class ["main_menu"] ] items))



let default ?(title) content =
  let page page_title main_menu auth_state html_stuff =
    Html5.(
      html
        (head (title (pcdata page_title)) [
          link ~rel:[`Icon] ()
            ~href:(uri_of_string (fun () ->
              "http://biology.as.nyu.edu/docs/TEMPLATE/1817/favicon.ico"));
          link ~rel:[`Stylesheet]
            ~href:(Html5.make_uri ~service:Services.(stylesheet ()) ()) ();
          link ~rel:[`Stylesheet]
            ~href:(Html5.make_uri
                     ~service:(Eliom_service.preapply
                                 ~service:(Eliom_service.static_dir ())
                                 ["hitscoreweb.css"])
                     ()) ();
        ])
        (body ~a:[ a_style "cursor: wait" ] [
          div ~a:[ a_class ["wrapper"] ] [
            div ~a:[ a_class ["top_banner"] ] [
              div ~a:[ a_class ["top_menu"] ] [
                a_link Services.default [pcdata "Home"] ();
                Option.value ~default:(span []) main_menu];
              div auth_state;
            ];
            div ~a:[ a_class ["main_page"]] [
              div html_stuff;
            ];
          ];
          div ~a:[ a_class ["footer"] ] [
            core_a
              ~a:[ a_hreff "http://biology.as.nyu.edu/object/biology.facilities.sequencing" ]
              [pcdata "Gencore"];
            pcdata " 2011 — 2012.  ";
            core_a ~a:[ a_hreff "http://www.ocaml.org" ] [pcdata "OCaml"];
            pcdata "/";
            core_a ~a:[ a_hreff "http://ocsigen.org" ] [pcdata "Ocsigen"];
            pcdata " powered.";
          ];
        ]))
  in
  let html_result =
    let page_title =
      Option.value_map title ~default:"Gencore" ~f:(sprintf "Gencore: %s")
    in
    Authentication.display_state ()
    >>= fun auth_state ->
    content >>= fun good_content ->
    menu_ul () >>= fun main_menu ->
    return (page page_title main_menu auth_state good_content)
  in
  let open Html5 in
  let error_page () =
    page "Error" None [] [
      h1 [ksprintf pcdata "Gencore: Error Page"];
      p [ksprintf pcdata "An error occurred on %s:"
            Time.(now () |> to_string)];
      p [pcdata "If you think this is not normal, please complain at ";
         codef "bio.gencore@nyu.edu";
         pcdataf "."];]
    |! Lwt.return
  in
  Lwt.bind html_result (function
  | Ok html ->
    ignore {unit{
      dbg "Last thing of the onload";
      Dom_html.document##body##style##cursor <- Js.string "auto" }};
    Lwt.return html
  | Error e ->
    let error_string = string_of_error e in
    Lwt.(
      logf "Generating error page because:\nError: %s\n\
            URL: %s\nUser-agent: %s\nFrom: %s\nRequest-id: %Ld"
        error_string
        (Eliom_request_info.get_full_url ())
        (Eliom_request_info.get_user_agent ())
        (Eliom_request_info.get_remote_ip ())
        (Eliom_request_info.get_request_id ())
      >>= fun _ ->
      ignore {unit{
        dbg "Last thing of the onload (error page)";
        Dom_html.document##body##style##cursor <- Js.string "auto" }};
      error_page ()))

type table_cell_html5 = Html5_types.flow5 Html5.elt list

type table_cell =
[ `head of Html5_types.span_content_fun Html5.elt list
| `head_cell of Msg.head_cell
| `text of table_cell_html5
| `sortable of string * table_cell_html5
| `number of (float -> string) * float
| `subtable of table_cell list list
| `with_geometry of int * int * table_cell
]

type content =
| Description of
    (Html5_types.phrasing Html5.elt * Html5_types.flow5 Html5.elt) list
| Section of Html5_types.phrasing Html5.elt * content
| List of content list
| Table of [`alternate_colors | `normal] * table_cell list list
| Paragraph of Html5_types.flow5 Html5.elt list

let content_description l = Description l
let content_description_opt l = Description (List.filter_opt l)
let content_section t c = Section (t, c)
let content_list l = List l
let content_table ?(transpose=false) ?(style=`normal) l =
  let t = function
    | [] -> []
    | hl :: tll ->
        (* let lgth = List.length hl in *)
      List.mapi hl (fun i h ->
        h :: (List.map tll (fun tl ->
          Option.value ~default:(`text []) (List.nth tl i))))
  in
  Table (style, if transpose then t l else l)

let cell_text s =
  let open Html5 in
  `sortable (s, [pcdataf "%s" s])
let cell_option o =
  cell_text (Option.value ~default:"—" o)

let cell_fmt fmt = ksprintf cell_text fmt
let cell_fmt_option fmt onearg =
  match onearg with
  | Some s -> cell_fmt fmt s
  | None -> cell_option None

let cell_timestamp t =
  let open Html5 in
  let s = Timestamp.to_string t in
  `sortable (s,
             [ span ~a:[a_title s]
                 [pcdata
                     (Time.(t |! to_local_date) |! Date.to_string)]])
let cell_timestamp_option = function
  | None -> cell_text "—"
  | Some t -> cell_timestamp t

let cell_int i =
  `sortable (sprintf "%d" i, [Html5.codef "%d" i])
let cell_int_option = function
  | None -> cell_text "—"
  | Some i -> `sortable (sprintf "%d" i, [Html5.codef "%d" i])

let content_paragraph l = Paragraph l

let _global_table_ids = ref 0

let td_on_click_to_sort do_something order cell_id idx id =
  let order_multiplier =
    match order with `normal -> 1 | `reverse -> -1 in
  {{fun ev ->
    if %do_something then (
      let tab =
        Js.coerce_opt
          (Dom_html.document##getElementById (Js.string %id))
          Dom_html.CoerceTo.table (fun _ -> assert false) in
      let rows = tab##rows in
      let get_cell_title r c =
        Js.Opt.(
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
        array.(i - 1) <- (get_cell_title i %idx,
                          Js.Optdef.option (Js.Opt.to_option rows##item(i)));
      done;
      Array.stable_sort (fun (x,_) (y,_) ->
        %order_multiplier *
          (try compare (int_of_string x) (int_of_string y)
           with e ->
             try compare (float_of_string x) (float_of_string y)
             with e ->
               String.compare x y)) array;
      for i = 1 to rows##length - 1 do
        Js.Optdef.iter (snd array.(i - 1)) (fun row ->
          tab##deleteRow(i);
          tab##insertRow(i)##innerHTML <- row##innerHTML);
      done;
    )
}}

let flatten_table l =
  let rec total_subtables_height row =
    List.fold_left row ~init:0 ~f:(fun current cell ->
      match cell with
      | `subtable l ->
        max current (List.fold_left l ~init:0
                       ~f:(fun cur row -> cur + total_subtables_height row))
      | flat_one -> max current 1)
  in
  List.mapi l (fun i row ->
    let max_height = total_subtables_height row in
    let after_the_row = ref [] in
    let the_row =
      List.map row (fun cell ->
        match cell with
        | `subtable [] -> [`text []]
        | `subtable [h] ->
          List.map h (fun a -> `with_geometry (max_height, 1, a))
        | `subtable (h :: t) ->
          after_the_row := t;
          h
        | any_other -> [`with_geometry (max_height, 1, any_other)])
      |! List.concat in
    (i, the_row :: !after_the_row)
  )

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
  | Table (_, []) -> div []
  | Table (style, h :: t) ->
    let rec make_cell ?(colspan=1) ?(rowspan=1) ?orderable idx cell =
      let really_orderable =
        (* Really orderable: if there is more than one sortable
           element in that column.
           AND there are no subtables*)
        List.length t > 1
        && List.exists (List.map t (fun l -> List.nth l idx))
          ~f:(function Some (`sortable _) | Some (`number _)-> true | _ -> false)
      in
      let cell_id = incr _global_table_ids; sprintf "cell%d" !_global_table_ids in
      let buttons =
        Option.value_map (if really_orderable then orderable else None)
          ~default:[]
          ~f:(fun tableid ->
            let td_onclick order =
              td_on_click_to_sort true order cell_id idx tableid in
            [
              span ~a:[
                a_title "sort:normal";
                a_class ["sort_normal_button"];
                a_onclick (td_onclick `normal);]
                [];
              span ~a:[
                a_title "sort:reverse";
                a_class ["sort_reverse_button"];
                a_onclick (td_onclick `reverse);]
                []; ])
      in
      match cell with
      | `head (c) ->
        td ~a:[a_id cell_id; a_class ["content_table_head"];
              a_rowspan rowspan; a_colspan colspan]
          ([span c] @ buttons)
      | `head_cell c ->
        td ~a:[a_id cell_id; a_title c#tooltip; a_class ["content_table_head"];
              a_rowspan rowspan; a_colspan colspan]
          ([span c#cell] @ buttons)
      | `sortable (title, cell) ->
        td  ~a:[ a_title title; a_class ["content_table_text"];
               a_rowspan rowspan; a_colspan colspan] cell
      | `text cell ->
        td  ~a:[ a_class ["content_table_text"];
               a_rowspan rowspan; a_colspan colspan] cell
      | `number (sof, f) ->
        let s = sof f in
        td  ~a:[ a_title s; a_class ["content_table_number"];
               a_rowspan rowspan; a_colspan colspan]
          [pcdataf "%s" (pretty_string_of_float ~sof f)]
      | `with_geometry (rowspan, colspan, cell) ->
        make_cell ~colspan ~rowspan ?orderable idx cell
      | `subtable [] ->
        td  ~a:[ a_class ["content_table_text"] ] []
      | `subtable (h :: t) ->
        td  ~a:[ a_class ["content_table_text"];
                 a_colspan (List.length h * colspan);
                 a_rowspan rowspan; ]
          [div [html_of_content (Table (`normal, h :: t))]]
    in
    let id = incr _global_table_ids; sprintf "table%d" !_global_table_ids in
    let flattened = flatten_table t in
    let orderable =
      if List.exists t (fun l -> List.exists l (function
      | `subtable s when List.length s > 1 -> true
      | _ -> false))
      then None
      else Some id in
    div [
      table
        ~a:[ a_id id;
             a_style "border: 3px  solid black; \
                        border-collapse: collapse; " ]
        (tr (List.mapi h (make_cell ?orderable)))
        (List.map flattened (fun (i, subrows) ->
          List.map subrows (fun l ->
            tr ~a:[ a_class
                      [if style = `alternate_colors && i mod 2 = 1 then
                          "odd_colored_row" else ""]]
              (List.mapi l (make_cell ?orderable:None)))) |! List.concat)
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

module Highchart = struct

  let make_curve_series curve =
    let categories =
      List.mapi curve ~f:(fun i _ -> sprintf "'%d'" (i + 1))
      |! String.concat ~sep:", " in
    let series =
      sprintf "{type: 'spline', name: 'Mean', data: [%s]}"
        (List.map curve ~f:(sprintf "%.2f") |! String.concat ~sep:", ") in
    (categories, series, List.fold_left curve ~f:max ~init:0.)

  let make_stack_series_exn stack =
    let categories =
      List.mapi stack ~f:(fun i _ -> sprintf "'%d'" (i + 1))
      |! String.concat ~sep:", " in
    let stack_series =
      let keys = List.hd_exn stack |! List.map ~f:fst |! List.rev in
      List.map keys ~f:(fun k ->
        List.map stack ~f:(fun i -> List.Assoc.find_exn i k |! sprintf "%.2f")
        |! String.concat ~sep:", "
        |! sprintf "{type: 'column', name: '%s', data: [%s]}" k)
      |! String.concat ~sep:" ,"
    in
    let y_max =
      List.fold_left stack ~init:0. ~f:(fun prev n ->
        max prev (List.map n snd |! List.fold_left ~f:(+.) ~init:0.)) in
    (categories, stack_series, y_max)


  let make_box_whisker_plot_exn by_5_list =
    let categories =
      List.mapi by_5_list ~f:(fun i _ -> sprintf "'%d'" (i + 1))
      |! String.concat ~sep:", " in
    let minimum      = List.map by_5_list (fun (x, _, _, _, _) -> x) in
    let fst_quartile = List.map by_5_list (fun (_, x, _, _, _) -> x) in
    let median       = List.map by_5_list (fun (_, _, x, _, _) -> x) in
    let trd_quartile = List.map by_5_list (fun (_, _, _, x, _) -> x) in
    let maximum      = List.map by_5_list (fun (_, _, _, _, x) -> x) in
    let series =
      let make_series n d =
        sprintf "{name: '%s', marker:{enabled:false},
                  data: [%s], type: 'scatter'}"
          n (List.map d (sprintf "%.2f") |! String.concat ~sep:", ") in
      String.concat ~sep:", " [
        make_series "Minimum" minimum;
        make_series "1st Quartile" fst_quartile;
        make_series "Median" median;
        make_series "3rd Quartile" trd_quartile;
        make_series "Maximum" maximum; ] in
    let y_max = List.fold_left maximum ~init:0. ~f:max in
    (categories, series, y_max)


  let additional_box_whisker_renderer = "
, function(chart) {
  var min = chart.series[0].data;
  var quartile1 = chart.series[1].data;
  var median = chart.series[2].data;
  var quartile3 = chart.series[3].data;
  var max = chart.series[4].data;

  var translate = 0;
  var semiwidth = 4;
  var fwidth = 2 * semiwidth;
  var stroke_width = 2;

  for(i=0; i<quartile1.length; i++)
  {

       chart.renderer.rect(quartile3[i].plotX-semiwidth+chart.plotLeft-translate,
                           quartile3[i].plotY+chart.plotTop,
                           fwidth,quartile1[i].plotY-quartile3[i].plotY, 0)
          .attr({
          'stroke-width': stroke_width,
          stroke: '#aaa',
          fill: '#ccc',
          zIndex:4
      })
      .add();

      chart.renderer.path(['M',max[i].plotX-semiwidth+chart.plotLeft-translate,
                          max[i].plotY+chart.plotTop,'L',
                          max[i].plotX+semiwidth+chart.plotLeft-translate,
                          max[i].plotY+chart.plotTop])
          .attr({
          'stroke-width': stroke_width,
          stroke: 'blue',
          zIndex:5
      })
      .add();

     chart.renderer.path(['M',median[i].plotX-semiwidth+chart.plotLeft-translate,
                         median[i].plotY+chart.plotTop,'L',
                         median[i].plotX+semiwidth+chart.plotLeft-translate,
                         median[i].plotY+chart.plotTop])
          .attr({
          'stroke-width': stroke_width,
          stroke: 'green',
          zIndex:5
      })
      .add();


     chart.renderer.path(['M',min[i].plotX-semiwidth+chart.plotLeft-translate,
                         min[i].plotY+chart.plotTop,'L',
                         min[i].plotX+semiwidth+chart.plotLeft-translate,
                         min[i].plotY+chart.plotTop])
          .attr({
          'stroke-width': stroke_width,
          stroke: 'red',
          zIndex:5
      })
      .add();

     chart.renderer.path(['M',min[i].plotX+chart.plotLeft-translate,
                     min[i].plotY+chart.plotTop,'L',
                     max[i].plotX+chart.plotLeft-translate,
                     max[i].plotY+chart.plotTop])
          .attr({
          'stroke-width': stroke_width,
          stroke: '#aaa',
          zIndex:3
      })
      .add();
  }
}"

  let box_whisker_tooltip = "
     tooltip:{
         formatter: function(){
             var s = '<b>'+this.x+'</b><br>';
             for(var i = this.points.length - 1; i > -1 ; i-- ){
                 colors = ['red','#444', 'green', '#444', 'blue'];
    s = s + ['<br>','<span style=\"color:' + colors[i] + '\">',
             this.points[i].series.name, '</span>: ',
      '<b>', Highcharts.numberFormat(this.points[i].y, 0), '</b>'].join('');
             }
             return s;
         },
         shared:true
     }
     "
  let standard_tooltip = "
       \     tooltip: {
       \       formatter: function() {
       \         return ''+ this.series.name +': '+ this.y;
       \       }
       \     }"

  let make_exn
      ?(with_legend=false) ?categories ?(more_y=4.) ?y_axis_title ~plot_title spec =
    let open Html5 in
    let plot_id = unique_id "plot" in
    let container_id = plot_id ^ "_container" in
    let width = ref 0 in
    let update_width l =
      width := max !width (List.length l * 18)
    in
    let highchart_script =
      let series =
        List.map spec (function
        | `curve c -> update_width c; make_curve_series c
        | `stack s -> update_width s; make_stack_series_exn s
        | `box_whisker bw ->
          update_width bw; make_box_whisker_plot_exn bw) in
      let series_string = List.map series ~f:snd3 |! String.concat ~sep:", " in
      let y_max = List.fold_left ~f:(fun c (_,_,y) -> max c y) ~init:0. series in
      let categories =
        match categories with
        | Some s -> s
        | None -> List.hd_exn series |! fst3 in
      let more_code, tooltip =
        match spec with
        | [`box_whisker _] -> (additional_box_whisker_renderer, box_whisker_tooltip)
        | l when List.exists l (function `box_whisker _ -> true | _ -> false) ->
          failwith "box_whisker-with-other-stuff: NOT IMPLEMENTED"
        | _ -> ("", standard_tooltip)
      in
      sprintf "
       \ var %s_chart;
       \ var %s_fun = (function() {
       \   %s_chart = new Highcharts.Chart({
       \     chart: {renderTo: '%s'},
       \     title: {text: '%s'},
       \     xAxis: {categories: [%s]},
       \     yAxis: {min: 0, max: %.0f %s},
       \     plotOptions: {column: {stacking: 'normal'}},
       \     %s,
       \     legend: {enabled: %b},
       \     series: [%s]
       \   }%s);
       \ }); "
        plot_id plot_id plot_id
        container_id plot_title categories
        (y_max +. more_y)
        (Option.value_map y_axis_title ~default:""
           ~f:(sprintf ", title: {text: '%s'}"))
        tooltip
        with_legend
        series_string
        more_code
    in
    let to_run = sprintf "
%s_fun();
" plot_id in
    dbg "dealing with %s" plot_id;
    make_unsafe_eval_string_onload ( to_run);
    [ script (cdata_script highchart_script);
      div ~a:[
        a_id container_id;
        (* a_onload {{ fun _ -> (Js.Unsafe.eval_string %to_run : unit);}}; *)
        ksprintf a_style "display: block; width: %dpx; height: 500px" !width
      ] []]


  let make ?with_legend ?categories ?(more_y=4.) ?y_axis_title ~plot_title spec =
    try
      return (make_exn ?with_legend ?categories ~more_y ?y_axis_title
                ~plot_title spec)
    with
      e ->
        error (`error_while_preparing_highchart (e, plot_title))

end


let hide_show_div ?(a=[]) ?(display_property="block")
    ?(start_hidden=true) ~show_message ~hide_message inside =
  let more_a = a in (* "a" will be hidden while opening Html5: *)
  let open Html5 in
  let the_div_id = unique_id "hide_show_div" in
  let the_msg_id = unique_id "hide_show_msg" in
  let initial_property = if start_hidden then "none" else display_property in
  let initial_message  = if start_hidden then show_message else hide_message in
  let span_msg =
    span ~a:[
      a_id the_msg_id;
      a_class ["like_link"];
      a_style "padding: 2px";
      a_onclick {{fun _ ->
        let the_div = get_element_exn %the_div_id in
        let the_msg = get_element_exn %the_msg_id in
        if the_div##style##display = Js.string "none"
        then (
          the_div##style##display <- Js.string %display_property;
          the_msg##innerHTML <- Js.string %hide_message;
        ) else (
          the_div##style##display <- Js.string "none";
          the_msg##innerHTML <- Js.string %show_message;
        )
      }} ]
      [pcdata initial_message] in
  (span_msg,
   div ~a:(a_id the_div_id
           :: ksprintf a_style "display: %s" initial_property
           :: more_a)
    inside)

let pretty_box content =
  let open Html5 in
  div ~a:[a_style "display: block" ] [
    div ~a:[ksprintf a_style
               "display: inline-block;
                border: 3px  solid %s;
                border-radius: 3px; " color_theme#title_violet]
      content]


{client{
  let string_map ~f s =
    let res = String.create (String.length s) in
    let c = ref 0 in
    String.iter (fun char ->
      res.[!c] <- f char;
      incr c)
      s;
    res

  let involution =
    string_map ~f:(fun char -> char_of_int ((int_of_char char + 128) mod 256))

}}
let involution =
  String.map ~f:(fun char -> char_of_int ((int_of_char char + 128) mod 256))

let anti_spam_mailto ~id ~mailto =
  let (encoded:string) = (involution mailto : string) in
  ignore {unit{
    let open Dom_html in
    begin match opt_tagged (document##getElementById (Js.string %id)) with
    | Some (A anchor) ->
      anchor##href <- Js.string (involution %encoded)
    | _ -> ()
    end
  }}
