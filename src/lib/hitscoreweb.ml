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

  let library_name =
    Eliom_services.service
      ~path:["library"] ~get_params:Eliom_parameters.(string "name") ()

  let library_project_name =
    Eliom_services.service
      ~path:["library"] 
      ~get_params:Eliom_parameters.(string "project" ** string "name") ()

  let persons =
    Eliom_services.service
      ~path:["persons"] ~get_params:Eliom_parameters.any ()

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

  let description l = Description l
  let description_opt l = Description (List.filter_opt l)
  let content_section t c = Section (t, c)
  let content_list l = List l
  let content_table l = Table l

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
          return Html5.(li [
            ksprintf pcdata "Ran on %s." (run_date |! 
                Time.to_local_date |! Date.to_string);
            br ();
            code [ksprintf pcdata "%s:%s" host hiseq_dir_name];
            br ();
            ksprintf pcdata "Run type : %ld%s%s"
              read_length_1
              (Option.value_map ~default:"" ~f:(sprintf "x%ld") read_length_index)
              (Option.value_map ~default:"" ~f:(sprintf "x%ld") read_length_2)
          ])))
      >>= fun l ->
      return Html5.(li [
        Eliom_output.Html5.a Services.flowcell [pcdata serial_name] serial_name;
        (if List.length l = 0 then div [pcdata "Never run."] 
         else div [ul l])
      ]))
    >>= fun ul ->
    return (List.length flowcells, ul)
  )
  >>= fun (length, items) ->
  return Html5.(div [
    h1 [ ksprintf pcdata "%d Flowcells" length];
    ul items
  ])

let person_link dbh person_t =
  Layout.Record_person.(
    cache_value ~dbh person_t >>| get_fields
    >>= fun { given_name; family_name; email; _ } ->
    return (
      Eliom_output.Html5.a Services.persons
        [ksprintf Html5.pcdata "%s %s" given_name family_name]
        ["filter", "true"; "email", email]))

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
            of_list_sequential (Array.to_list contacts) (person_link dbh)
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
              [
                `text   [ksprintf pcdata "Lane %d" !lane];
                `number [opt seeding_concentration_pM pcf];
		`text   [opt total_volume pcf];
		`text   (List.map people (fun html5 -> [ html5; br () ])
                                                       |! List.flatten);
                `text   (List.map libs 
                           (function
                           | (l, None) ->
                             [Eliom_output.Html5.a Services.library_name
                                 [pcdata l] l]
                           | (l, Some p) -> 
                             let name = sprintf "%s.%s" p l in
                             [Eliom_output.Html5.a Services.library_project_name
                                 [pcdata name] (p, l)])
                             |! interleave_list ~sep:[pcdata ", "] 
                             |! List.flatten);
              ]))))
    in
    lanes >>= fun lanes ->
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
          li [Eliom_output.Html5.a Services.flowcells [pcdata "Flowcells"] ()];
          li [Eliom_output.Html5.a Services.persons [pcdata "Persons"] []];

        ];
        p [pcdata (sprintf "Histcore's default web page: %s"
                     Time.(now () |> to_string))];
      ]))

let library  ~name ?project hsc =
  let full_name =
    match project with None -> name | Some p -> sprintf "%s.%s" p name in
  let sample_info dbh s =
    Layout.Record_sample.(
      cache_value ~dbh s >>| get_fields
      >>= fun { name; organism; note } ->
      let sample_name = name in
      let sample_note = note in
      match organism with
      | Some o ->
        Layout.Record_organism.(
          cache_value ~dbh o >>| get_fields
          >>= fun {name; informal; note } ->
          return (sample_name, sample_note, name, informal, note))
      | None -> return (sample_name, sample_note, None, None, None)) in
  Hitscore_lwt.db_connect hsc
  >>= fun dbh ->
  let stocks =
    Layout.Search.record_stock_library_by_name_project ~dbh name project in
  stocks >>= fun sl_list ->
  let stock_nb = List.length sl_list in
  of_list_sequential sl_list (fun slt ->
    Layout.Record_stock_library.(
      cache_value ~dbh slt >>| get_fields
      >>= fun { 
  	name; project; sample; protocol; application; stranded;
  	truseq_control; rnaseq_control;
  	barcode_type; barcodes; custom_barcodes;
        p5_adapter_length; p7_adapter_length; preparator; note;
      } ->
      Display_service.(Html5.(
        let opt_item o name tos =
          Option.map o (fun x -> (pcdata name, pcdata (tos x))) in
        let sample_information =
          of_option  sample (fun s ->
            sample_info dbh s
            >>= fun (sname, snote, oname, oinformal, onote) ->
            return (
              (pcdata "Sample info",
               span [
                 code [ksprintf pcdata "%S" sname];
                 Option.value_map snote ~default:(pcdata "")
                        ~f:(fun n -> ksprintf pcdata " (%s)" n);
                 Option.value_map oname ~default:(em [pcdata " (no organism)"])
                   ~f:(fun n -> ksprintf pcdata " from %S" n);
                 Option.value_map oinformal ~default:(pcdata "")
                        ~f:(fun n -> ksprintf pcdata " (%s)" n);
                 Option.value_map onote ~default:(pcdata "")
                   ~f:(fun n -> ksprintf pcdata " (%s)" n);
               ]))) in
        sample_information >>= fun sample_information ->
        of_option preparator (person_link dbh) >>= fun preparator_link ->
        return (
          content_section 
            (ksprintf pcdata "%s:" full_name)
            (description_opt [
              sample_information;
              opt_item application "Application" (sprintf "%S");
              Some (pcdata "Stranded", ksprintf pcdata "%b" stranded);
              Some (pcdata "TruSeq Control", ksprintf pcdata "%b" truseq_control);
              opt_item rnaseq_control "RNA-seq control" (sprintf "%s");
              Option.map preparator_link (fun l ->
                (pcdata "Prepared by", l));
              opt_item note "Note" (sprintf "%s");
            ]))))))
  >>= fun lib_info ->
  return Display_service.(Html5.(
    content_section 
      (ksprintf pcdata "Found %d librar%s:"
         stock_nb (if stock_nb = 1 then "y" else "ies"))
      (content_list lib_info)))
    


let () =

  let hitscore_configuration = Hitscore_lwt.Configuration.configure () in

  Eliom_services.register_eliom_module
    "hitscoreweb" 
    (fun () ->
      Eliom_output.Html5.register ~service:Services.default (fun () () ->
        default_service hitscore_configuration);

      Eliom_output.Html5.register ~service:Services.flowcells (fun () () ->
        flowcells_service hitscore_configuration);

      Eliom_output.Html5.register ~service:Services.persons
        (fun plist () ->
          let highlight =
            List.filter_map plist (function "email", e -> Some e | _ -> None) in
          let filter = List.exists plist ((=) ("filter","true")) in
          Display_service.make ~hsc:hitscore_configuration
            ~main_title:"People"
            (persons ~highlight ~filter hitscore_configuration));

      Eliom_output.Html5.register ~service:Services.flowcell
        (fun (serial_name) () ->
          Display_service.make ~hsc:hitscore_configuration
            ~main_title:"Flowcell"
            (one_flowcell hitscore_configuration ~serial_name));

      Eliom_output.Html5.register ~service:Services.library_name
        (fun (name) () ->
          Display_service.make ~hsc:hitscore_configuration
            ~main_title:"Library" (library ~name hitscore_configuration));

      Eliom_output.Html5.register ~service:Services.library_project_name
        (fun (project, name) () ->
          Display_service.make ~hsc:hitscore_configuration
            ~main_title:"Library" (library hitscore_configuration ~name ~project));

    )

