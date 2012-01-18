open Hitscoreweb_std

module Queries = 
  Hitscoreweb_queries.Make (Hitscore_lwt.Result_IO) (Hitscore_lwt.Layout.PGOCaml)

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

  let persons =
    Eliom_services.service
      ~path:["persons"] 
      ~get_params:Eliom_parameters.(opt (bool "filter") ** set string "email") ()

  let libraries =
    Eliom_services.service
      ~path:["libraries"] 
      ~get_params:Eliom_parameters.(set string "qualified_name") ()


end

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
  let content_table l = Table l
  let paragraph l = Paragraph l

  let error_page msg =
    Html5.(html
             (head (title (pcdata "ERROR; Hitscore Web")) [])
             (body [
               h1 [ksprintf pcdata "Histcore's Error Page"];
               p [ksprintf pcdata "An error occurred on %s:"
                     Time.(now () |> to_string)];
               div msg;
               p [pcdata "Please complain at bio.gencore@nyu.edu."];
             ]))

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
        | `head c -> td ~a:[ a_style "border: 1px  solid black; color: red" ] c
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

  let make ~hsc ~main_title content =
    let html_content = 
      content
      >>= fun content ->
      return
        Html5.(html
                 (head (title (ksprintf pcdata "Hitscoreweb: %s" main_title)) [])
                 (body [
                   div [
                     Eliom_output.Html5.a Services.default [pcdata "Home"] ()
                   ];
                   hr ();
                   h1 [ksprintf pcdata "Hitscoreweb: %s" main_title];
                   html_of_content content           
                 ])) in
    let open Html5 in
    Lwt.bind html_content (function
    | Ok html ->
      Lwt.return html
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


end


let error_page msg =
  Html5.(html
           (head (title (pcdata "ERROR; Hitscore Web")) [])
           (body [
             p [pcdata (sprintf "Histcore's error web page: %s"
                          Time.(now () |> to_string))];
             p [ksprintf pcdata "Error: %s" msg];
           ]))

let flowcells hsc =
  let open Html5 in
  Hitscore_lwt.db_connect hsc
  >>= fun dbh ->
  Layout.Record_flowcell.(
    get_all ~dbh
    >>= fun flowcells ->
    of_list_sequential flowcells ~f:(fun f ->
      cache_value ~dbh f >>| get_fields
      >>= fun {serial_name; lanes} ->
      Layout.Search.record_hiseq_raw_by_flowcell_name ~dbh serial_name
      >>= fun dirs ->
      of_list_sequential dirs ~f:(fun d ->
        Layout.Record_hiseq_raw.(
          cache_value ~dbh d >>| get_fields
          >>= fun {read_length_1; read_length_index; read_length_2; 
                   run_date; host; hiseq_dir_name} ->
          return (li [
            ksprintf pcdata "Ran on %s, " (run_date |! 
                Time.to_local_date |! Date.to_string);
            code [pcdata (Filename.basename hiseq_dir_name)];
            ksprintf pcdata " (%ld%s%s)."
              read_length_1
              (Option.value_map ~default:"" ~f:(sprintf "x%ld") read_length_index)
              (Option.value_map ~default:"" ~f:(sprintf "x%ld") read_length_2)
          ])))
      >>= fun l ->
      return Display_service.(
        paragraph [
          strong [
            (Eliom_output.Html5.a Services.flowcell [pcdata serial_name] serial_name);
            pcdata ":"];
          if List.length l = 0 then span [pcdata " Never run."] else ul l
        ]))
    >>= fun ul ->
    Hitscore_lwt.db_disconnect hsc dbh
    >>= fun _ ->
    return Display_service.(
      content_section
        (ksprintf pcdata "Found %d Flowcells" (List.length ul))
        (content_list ul)
    ))
    
let person_essentials dbh person_t =
  Layout.Record_person.(
    cache_value ~dbh person_t >>| get_fields
    >>= fun { given_name; family_name; email; _ } ->
    return (given_name, family_name, email))

let person_link dbh person_t =
  person_essentials dbh person_t >>= fun (f, l, e) ->
  return (Eliom_output.Html5.a Services.persons
            [ksprintf Html5.pcdata "%s %s" f l]
            (Some true, [e]))


let persons ?(filter=false) ?(highlight=[]) hsc =
  Hitscore_lwt.db_connect hsc
  >>= fun dbh ->
  Layout.Record_person.(
    get_all ~dbh >>= fun plist ->
    let rows_m =
      of_list_sequential plist (fun p ->
        cache_value ~dbh p >>| get_fields
        >>= fun {print_name; given_name; middle_name; family_name;
  	         email; login; nickname; note;} ->
        let opt f m = Option.value_map ~f ~default:(f "") m in
        let is_vip = List.exists highlight ((=) email) in
        if not is_vip && filter then
          return None
        else Html5.(
          let email_field =
            let style = if is_vip then "color: green" else "" in
            `text [code ~a:[a_id email; a_style style] [pcdata email]]
          in
          return (Some [
            `text [opt pcdata print_name];
            `text [pcdata given_name];
            `text [opt pcdata middle_name];
            `text [pcdata family_name];
            email_field;
            `text [code [opt pcdata login]];
            `text [opt pcdata nickname];
            `text [opt pcdata note];]))) in
    rows_m >>= fun rows ->
    Hitscore_lwt.db_disconnect hsc dbh
    >>= fun _ ->
    let actual_rows = List.filter_opt rows in
    let nrows = List.length actual_rows in
    return Display_service.(Html5.(
      content_section 
        (ksprintf pcdata "Found %d Person%s" nrows
           (if nrows > 1 then "s" else ""))
        (content_table 
           ([`head [pcdata "Print name"];
	     `head [pcdata "Given name"];
	     `head [pcdata "Middle name"];
	     `head [pcdata "Family name"];
	     `head [pcdata "Email"];
	     `head [pcdata "Login"];
	     `head [pcdata "Nickname"];
	     `head [pcdata "Note"];]
            :: actual_rows)))))

let one_flowcell hsc ~serial_name =
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
            of_list_sequential (Array.to_list contacts) (person_essentials dbh)
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
              let opt o f = Option.value_map ~default:na o ~f in
              let pcf = (ksprintf pcdata "%.0f") in
              let people_cell = 
                (List.map people (fun (f, l, e) ->
                  [ 
                    Eliom_output.Html5.a Services.persons
                      [ksprintf Html5.pcdata "%s %s" f l] (Some true, [e]);
                    br () ]) |! List.flatten)
                @ [
                  if List.length people > 1 then
                    small [
                      pcdata "(";
                      Eliom_output.Html5.a Services.persons [pcdata "all"]
                        (Some true, List.map people (fun (f, l, e) -> e));
                      pcdata ")"
                    ]
                  else
                    span []
                ]
              in
              let libs_cell =
                let qnames =
                  List.map libs (function (l, None) -> l
                  | (l, Some p) -> sprintf "%s.%s" p l) in
                (List.map qnames (fun qn ->
                  Eliom_output.Html5.a Services.libraries [pcdata qn] [qn])
                  |! interleave_list ~sep:(pcdata ", "))
                @ [
                  if List.length qnames > 1 then
                    small [
                      pcdata " (";
                      Eliom_output.Html5.a Services.libraries [pcdata "all"] qnames;
                      pcdata ")"
                    ]
                  else
                    span []
                ]
              in
              [
                `text   [ksprintf pcdata "Lane %d" !lane];
                `number [opt seeding_concentration_pM pcf];
		`text   [opt total_volume pcf];
		`text   people_cell;
                `text   libs_cell
              ]))))
    in
    lanes >>= fun lanes ->
    Hitscore_lwt.db_disconnect hsc dbh
    >>= fun _ ->
    return Display_service.(Html5.(
      content_section 
        (ksprintf pcdata "Flowcell %s" serial_name)
        (content_table 
           ([ `head [pcdata "Lane Nb"]; 
	      `head [pcdata "Seeding C."];
	      `head [pcdata "Vol."];
	      `head [pcdata "Contacts"];
	      `head [pcdata "Libraries"];]
            :: lanes))))
  | [] ->
    error (`no_flowcell_named serial_name)
  | more ->
    error (`layout_inconsistency (`record_flowcell, 
                                  `more_than_one_flowcell_called serial_name))

let default_service hsc =
  Lwt.return Html5.(
    html
      (head (title (pcdata "Hitscoreweb: Default")) [])
      (body [
        h1 [pcdata "Hitscoreweb: Home"];
        h2 [pcdata "Services"];
        ul [
          li [Eliom_output.Html5.a Services.flowcells [pcdata "Flowcells"] ()];
          li [Eliom_output.Html5.a Services.persons [pcdata "Persons"] (None, [])];
          li [Eliom_output.Html5.a Services.libraries [pcdata "Libraries"] []];

        ];
        p [pcdata (sprintf "Histcore's default web page: %s"
                     Time.(now () |> to_string))];
      ]))

let libraries ?(qualified_names=[]) hsc =
  let open Html5 in
  Hitscore_lwt.db_connect hsc
  >>= fun dbh ->
  Queries.full_libraries dbh
  >>| List.filter ~f:(fun (idopt, name, project, app, 
                           stranded, truseq, rnaseq,
                           bartype, barcodes, bartoms,
                           p5, p7, note,
                           sample_name, org_name,
                           prep_email, protocol) ->
    let qualified_name =
      let opt = Option.value ~default:"" in
      sprintf "%s.%s" (opt project) (opt name) in
    qualified_names = [] 
    || List.exists qualified_names
      ~f:(fun qn -> qualified_name = qn || Some qn = name))
  >>= fun lib_resulst ->
  of_list_sequential lib_resulst 
    ~f:(fun (idopt, name, project, app, 
             stranded, truseq, rnaseq,
             bartype, barcodes, bartoms,
             p5, p7, note,
             sample_name, org_name,
             prep_email, protocol) ->
      let lib_id = Option.value ~default:0l idopt in
      Queries.library_submissions ~lib_id dbh
      >>= fun submissions ->
      let opt f o = Option.value_map ~default:(pcdata "") ~f o in
      let person e =
        (Eliom_output.Html5.a Services.persons
           [ksprintf Html5.pcdata "%s" e] (Some true, [e])) in
      let submissions_cell = 
        let how_much =
          match List.length submissions with
          | 1 -> "Once" | 2 -> "Twice" | n -> sprintf "%d times" n in
        let flowcells = 
          List.map submissions fst |! List.dedup in
        (ksprintf pcdata "%s: " how_much)
        ::
          interleave_list ~sep:(pcdata ", ")
          (List.map flowcells (fun fcid ->
            let lanes = 
              List.filter submissions ~f:(fun (f,l) -> f = fcid) |! List.length in
            span [
              Eliom_output.Html5.a Services.flowcell
                [ksprintf pcdata "%s" fcid] fcid;
              ksprintf pcdata " (%d lane%s)"
                lanes (if lanes > 1 then "s" else "");
            ]))
        @ [pcdata "."]
      in
      let barcodes_cell =
        let barcodes_list =
          String.concat ~sep:"," 
            (List.map ~f:(sprintf "%ld") 
               (Array.to_list (Option.value ~default:[| |] barcodes))) in
        let custom_barcodes =
          match bartoms with
          | None | Some [| |] -> return [pcdata ""]
          | Some a -> 
            let l = Array.to_list a in
            Layout.Record_custom_barcode.(
              of_list_sequential l (fun id ->
                cache_value ~dbh {id} >>| get_fields
                >>= fun {position_in_r1; position_in_r2; 
                         position_in_index; sequence} ->
                return [ br ();
                         ksprintf pcdata "%s(%s)"
                           sequence
                           (["R1", position_in_r1; 
                             "I", position_in_index; "R2", position_in_r2]
                            |! List.filter_map ~f:(function
                              | _, None -> None
                              | t, Some i -> Some (sprintf "%s:%ld" t i))
                            |! String.concat ~sep:",")]))
            >>= fun pcdatas ->
            return (List.flatten pcdatas)
        in
        custom_barcodes >>= fun custom ->
        let non_custom =
          Option.value_map bartype
            ~default:(pcdata "NO BARCODE TYPE!") ~f:(fun t ->
              match Layout.Enumeration_barcode_provider.of_string t with
              | Ok `none -> pcdata ""
              | Ok `bioo -> ksprintf pcdata "BIOO[%s]" barcodes_list
              | Ok `bioo_96 -> ksprintf pcdata "BIOO-96[%s]" barcodes_list
              | Ok `illumina -> ksprintf pcdata "ILLUMINA[%s]" barcodes_list
              | Ok `nugen -> ksprintf pcdata "NUGEN[%s]" barcodes_list
              | Ok `custom -> strong [pcdata "CUSTOM"]
              | Error _ -> strong [pcdata "PARSING ERROR !!!"]
            )
        in
        return (non_custom :: custom)
      in
      barcodes_cell >>= fun barcoding ->
      return [
        `text [opt pcdata name]; `text [opt pcdata project];
        `text submissions_cell;
        `text [opt pcdata sample_name]; `text [opt pcdata org_name];
        `text [opt person prep_email]; `text [opt pcdata protocol];
        `text [opt pcdata app];
        `text [opt (ksprintf pcdata "%b") stranded];
        `text [opt (ksprintf pcdata "%b") truseq];
        `text [opt pcdata rnaseq];
        `text barcoding;
        `text [opt (ksprintf pcdata "%ld") p5];
        `text [opt (ksprintf pcdata "%ld") p7];
        `text [opt pcdata note];
      ])
  >>= fun rows ->
  let nb_rows = List.length rows in
  return Display_service.(
    content_section
      (ksprintf pcdata "Found %d librar%s:"
         nb_rows (if nb_rows = 1 then "y" else "ies"))
      (content_table 
         ([ `head [pcdata "Name"]; 
	    `head [pcdata "Project"];
            `head [pcdata "Submitted"];
            `head [pcdata "Sample-name"];
            `head [pcdata "Organism"];
            `head [pcdata "Preparator"];
            `head [pcdata "Protocol"];
            `head [pcdata "Application"];
            `head [pcdata "Stranded"];
            `head [pcdata "Truseq-control"];
            `head [pcdata "RNASeq-control"];
            `head [pcdata "Barcoding"];
            `head [pcdata "P5 Adapter Length"];
            `head [pcdata "P7 Adapter Length"];
            `head [pcdata "Note"];
          ] :: rows)))
    

let () =

  let hitscore_configuration = Hitscore_lwt.Configuration.configure () in

  Eliom_services.register_eliom_module
    "hitscoreweb" 
    (fun () ->
      Eliom_output.Html5.register ~service:Services.default (fun () () ->
        default_service hitscore_configuration);

      Eliom_output.Html5.register ~service:Services.flowcells
        (fun () () ->
          Display_service.make ~hsc:hitscore_configuration
            ~main_title:"Flowcells"
            (flowcells hitscore_configuration));

      Eliom_output.Html5.register ~service:Services.persons
        (fun (filter, highlight) () ->
          Display_service.make ~hsc:hitscore_configuration
            ~main_title:"People"
            (persons ~highlight ?filter hitscore_configuration));

      Eliom_output.Html5.register ~service:Services.libraries
        (fun qualified_names () ->
          Display_service.make ~hsc:hitscore_configuration
            ~main_title:"Libraries"
            (libraries ~qualified_names hitscore_configuration));

      Eliom_output.Html5.register ~service:Services.flowcell
        (fun (serial_name) () ->
          Display_service.make ~hsc:hitscore_configuration
            ~main_title:"Flowcell"
            (one_flowcell hitscore_configuration ~serial_name));

    )

