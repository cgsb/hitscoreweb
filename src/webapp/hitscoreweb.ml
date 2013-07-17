open Hitscoreweb_std_server

module Msg = Hitscoreweb_messages

module Web_data_access = Hitscoreweb_data_access

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

module Layout_service = Hitscoreweb_layout_service

module One_person_service = Hitscoreweb_one_person_service


module Evaluations_service = struct

  let b2f_section dbh layout =
    layout#assemble_sample_sheet#all >>= fun all_assemblies ->
    layout#bcl_to_fastq#all
    >>| List.stable_sort ~cmp:(fun a b -> compare b#g_inserted a#g_inserted)
    >>= fun b2fs ->
    while_sequential b2fs ~f:(fun b2f ->
      b2f#raw_data#get >>= fun hiseq_raw ->
      let assembly =
        List.find all_assemblies ~f:(fun g ->
          match g#g_result with
          | None -> false
          | Some r -> r#id = b2f#sample_sheet#id) in
      return (b2f, hiseq_raw, assembly))
    >>= fun b2fs ->
    while_sequential b2fs (fun (b2f, hiseq_raw, assembly) ->
      let open Template in
      return [
        cell_int b2f#g_id;
        cell_text (Sql_query.status_to_string b2f#g_status);
        cell_timestamp b2f#g_inserted;
        cell_timestamp_option b2f#g_started;
        cell_timestamp_option b2f#g_completed;
        cell_text hiseq_raw#flowcell_name;
        cell_int b2f#mismatch;
        cell_text b2f#version;
        cell_option b2f#tiles;
        cell_option b2f#bases_mask;
        cell_option
          Option.(map assembly
                    ~f:(fun a ->
                      Layout.Enumeration_sample_sheet_kind.to_string a#kind));
      ])
    >>= fun rows ->
    return Template.(
      let tab =
        content_table (
          [`head [Html5.pcdata "Id"];
           `head [Html5.pcdata "Status"];
           `head [Html5.pcdata "Inserted"];
           `head [Html5.pcdata "Started"];
           `head [Html5.pcdata "Completed"];
           `head [Html5.pcdata "Flowcell"];
           `head [Html5.pcdata "Mismatch"];
           `head [Html5.pcdata "Version"];
           `head [Html5.pcdata "Tiles Option"];
             `head [Html5.pcdata "Bases-Mask Option"];
             `head [Html5.pcdata "Kind of sample-sheet"];
            ] :: rows)
        in
        content_section (Html5.pcdataf "Bcl_to_fastq evaluations: %d"
                           (List.length b2fs)) tab)

  let fxqs_section dbh layout =
    layout#fastx_quality_stats#all
    >>| List.stable_sort ~cmp:(fun a b -> compare b#g_inserted a#g_inserted)
    >>= fun fxqss ->
    while_sequential fxqss ~f:(fun fxqs ->
      fxqs#input_dir#get >>= fun gf ->
      let configuration = Configuration.configure ~root_path:"root:/" () in
      Common.path_of_volume ~configuration ~dbh gf#directory#pointer
      >>= fun path ->
      return (path, fxqs))
    >>= while_sequential ~f:(fun (path, fxqs) ->
      let open Template in
      return [
        cell_int fxqs#g_id;
        cell_text (Sql_query.status_to_string fxqs#g_status);
        cell_timestamp fxqs#g_inserted;
        cell_timestamp_option fxqs#g_started;
        cell_timestamp_option fxqs#g_completed;
        cell_text path;
        cell_int fxqs#option_Q;
        cell_option fxqs#filter_names;
      ])
    >>= fun rows ->
    return Template.(
      let tab =
        content_table (
          [`head [Html5.pcdata "Id"];
           `head [Html5.pcdata "Status"];
           `head [Html5.pcdata "Inserted"];
           `head [Html5.pcdata "Started"];
           `head [Html5.pcdata "Completed"];
           `head [Html5.pcdata "Input path"];
           `head [Html5.pcdata "Option Q"];
           `head [Html5.pcdata "Filter Names"];
          ] :: rows)
        in
        content_section (Html5.pcdataf
                           "Fastx_quality_stats evaluations: %d"
                           (List.length fxqss)) tab)

  let evaluations configuration =
    with_database ~configuration (fun ~dbh ->
      let layout = Classy.make dbh in
      b2f_section dbh layout >>= fun b2f ->
      fxqs_section dbh layout >>= fun fxqs ->
      return Template.(content_list [b2f; fxqs])
    )

  let make ~configuration =
    (fun () () ->
      let main_title = "Function Evaluations" in
      Template.default ~title:main_title
        (Authentication.authorizes (`view `all_evaluations)
         >>= function
         | true ->
           Template.make_content ~configuration ~main_title
             (evaluations configuration);
         | false ->
           Template.make_authentication_error ~configuration ~main_title
             (return [Html5.pcdataf
                         "You may not view the function evaluations."])))


end

module All_or_any_fastx_stats = struct

  open Template

  let all_paths_page ~configuration =
    with_database ~configuration begin fun ~dbh ->
      let layout = Classy.make dbh in
      layout#fastx_quality_stats#all
      >>| List.filter_map ~f:(fun fxqs ->
        Option.map fxqs#g_result (fun pointer -> (fxqs, pointer)))
      >>= fun successful_fxqss ->
      while_sequential successful_fxqss ~f:(fun (fxqs, result) ->
        result#get
        >>= fun dir ->
        Common.path_of_volume ~configuration ~dbh dir#directory#pointer
        >>= fun path ->
        let for_display_configuration =
          Configuration.configure ~root_path:"root:/" () in
        Common.path_of_volume ~configuration:for_display_configuration
          ~dbh dir#directory#pointer
        >>= fun display_path ->
        (* eprintf "some path: %s\n%!" path; *)
        ksprintf Sequme_flow_sys.get_system_command_output
          "find %s -name '*.fxqs' -type f -print" (Filename.quote path)
        >>< begin function
        | Ok (out, err) ->
          let files = String.split ~on:'\n' out |> List.map ~f:String.strip
                      |> List.filter ~f:(fun s -> s <> "") in
          return files
        | Error (`system_command_error (s, status)) ->
          ksprintf log "Find in %s error:\n%S\n%s" path s
            (match status with
            | `exited   i -> sprintf "exited   %d" i
            | `exn      e -> sprintf "exn      %s" (Exn.to_string e)
            | `signaled i -> sprintf "signaled %d" i
            | `stopped  i -> sprintf "stopped  %d" i)
          >>= fun () ->
          return []
        end
        >>= fun paths ->
        return (display_path, paths))
      >>= fun result_paths ->
      let open Template in
      let sections =
        List.map
          (List.sort ~cmp:(fun (a, _) (b, _) -> String.compare b a) result_paths)
          (fun (display_path, paths) ->
            let open Html5 in
            let paragraph =
              List.map (List.sort ~cmp:String.compare paths) (fun path ->
                li [Template.a_link Services.fastx_results
                      [codef "%s" (Filename.basename path)] (Some path)]
              ) in
            content_section (Html5.pcdataf "In %s" display_path)
              (content_paragraph [ul paragraph])) in
      return Template.(content_list sections)
    end

  let one_path ~configuration path =
    Hitscoreweb_libraries.rendered_fastx_table path
    >>= fun table ->
    Hitscoreweb_libraries.fastx_quality_plots path
    >>= fun plots ->
    let open Template in
    let sections =
      Option.value_map table
        ~default:(content_section (Html5.pcdataf "No TABLE") (content_list []))
        ~f:(fun tab ->
          content_section (Html5.pcdataf "Table") (content_paragraph tab))
      ::
        List.map plots
          ~f:(fun pl ->
            content_section (Html5.pcdataf "Plots")
              (content_paragraph [pl]))
    in
    return (content_list sections)


  let display configuration = function
  | Some path ->
    one_path ~configuration path
    >>= fun content ->
    let page =
      content_section
        (Html5.pcdataf "Fastx Result %s" (Filename.basename path)) content
    in
    return page
  | None ->
    all_paths_page ~configuration
    >>= fun content ->
    let page = content_section (Html5.pcdataf "All Fastx Results") content in
    return page

  let make ~configuration =
    (fun selection () ->
      let main_title = "All Fastx Results" in
      Template.default ~title:main_title (
        Authentication.spy_userf "Visit fastx results (%s)"
          (Option.value selection ~default:"<NONE>")
        >>= fun () ->
        Authentication.authorizes (`view `all_evaluations)
        >>= function
        | true ->
          Template.make_content ~configuration ~main_title
            (display configuration selection);
        | false ->
          Template.make_authentication_error ~configuration ~main_title
            (return [Html5.pcdataf
                       "You may not view the function evaluations."])))


end

module Test_service = struct

  let test_form ~state =
    let open Hitscoreweb_meta_form in
    let upload_many_files_store = Upload.fresh_store [] in
    let upload_one_file_store = Upload.fresh_store [] in
    let services_form =
      ref (
        let identifier_friendly =
          ("non-empty string of letters, numbers, underscores, and dashes",
           "[a-zA-Z0-9_-]+") in
        let strictly_positive = Range.(make (exclusive 0.) infinity) in
        let percentage = Range.(make (inclusive 1.) (inclusive 100.)) in
        let open Form in
        make ~text_buttons:["Submit …"; "Cancel"; "Trigger Error"]
          (section Markup.([text "First Section"]) [
            integer ~question:[Markup.text "Pick an integer"] ~value:42 ();
            (let question =
               Markup.([text "Pick a string "; italic "(regular expression)"]) in
             string ~regexp:identifier_friendly ~question ());
            section Markup.([text "Subsection"]) [
              string
                ~help:Markup.(par [text "HHEEEELLLPPP"])
                ~text_question:"Pick a string" ~value:"sldk jskd" ();
              date ~text_question:"A date:" ~value:"21/12/2012" ();
              date ~text_question:"Another date:" ();
              upload ~store:upload_many_files_store
                Markup.([text "Upload many FILEs ! "; italic "pleaaase"]);
              upload ~store:upload_one_file_store  ~multiple:false
                Markup.([text "Upload one FILE ! "; italic "pleaaase"]);
              float  ~text_question:"Now a float:" ~value:(atan (-1.)) ();
              float  ~text_question:"percent float" ~range:percentage ();
              float  ~text_question:"float > 0." ~range:strictly_positive ();
              integer ~text_question:"int > 0" ~range:strictly_positive ();
              string_enumeration ~question:Markup.([text "Many strings?"])
                ~value:"one" ["zero"; "one"; "two"; "three"];
              open_string_enumeration ~question:Markup.([text "Many strings?"])
                ~value:"one" ~other:"Make up another one …"
                ["zero"; "one"; "two"; "three"];
              begin
                let make_sub ?name ?age () =
                  section Markup.([text "Create a new person"]) [
                    string  ~text_question:"person's name" ?value:name ();
                    integer ~text_question:"person's age" ?value:age ();
                  ] in
                meta_enumeration
                  ~help:Markup.(list [par [text "some help"]; par [italic "more help"]])
                  ~overall_question:Markup.([text "Please choose or create a person"])
                  ~creation_cases:[
                    ("Create person …", make_sub ());
                    ("Create ageless person …",
                     section Markup.([text "Create an ageless person"]) [
                       string  ~text_question:"person's name" ~value:"LA Woman" ();
                     ]);
                  ]
                  ~choice:"the first"
                  [("the first", make_sub ~name:"The First" ~age:42 ());
                   ("anotherone", make_sub ~name:"Another One" ~age:45 ());
                   ("notthefirst", make_sub ~name:"Not The First" ~age:22 ());]
              end;
              section Markup.([text "Extensible List:"]) [begin
                let make_model sec =
                  section Markup.([ text sec ]) [
                    string ~text_question:"Pick a name" ();
                    string_enumeration ~question:Markup.([text "Pick a ";
                                                          italic "type:"])
                      ["int"; "float"];
                  ] in
                extensible_list ~question:Markup.([italic "Add a thing"])
                  ~model:(make_model "New thing")
                  (List.init 2 (ksprintf make_model "%d one"))
              end];
            ];
          ])
      ) in
    create ~state
      Form.(function
      | None ->
        return (make ~text_buttons:["Start the form"] empty)
      | Some {form_content = Empty; _} ->
        return (!services_form)
      | Some ({form_content = (Section (title, modified_form));} as form)
          when title = [Markup.text "First Section"] ->
        begin match form.form_choice with
        | Some 0 ->
          services_form := `form form;
          (* dbg "Modified form : %s" *)
            (* Deriving_Json.(to_string Json.t<Hitscoreweb_meta_form.form_content> modified_form); *)
          dbg "Files:[\n TODO \n]";
          return (make ~text_buttons:["Would you like to restart?"] empty)
        | Some 1 ->
          dbg "User clicked Cancel";
          return (make ~text_buttons:["Send"]
                    (string ~text_question:"Why did you cancel???" ()))
        | Some 2 ->
          dbg "User clicked to trigger an error";
          error [Markup.text "TRIGGERED ERROR !!"]
        | _ ->
          dbg "unexpected input";
          error [Markup.text "UNEXPECTED ERROR !!"]
        end
      | Some _ ->
        return (make ~text_buttons:["Nothing to save?"] empty)
      )


  let test ~state =
    let open Html5 in
    begin
      let welcome = [
        h2 [pcdata "Welcome"];
        h3 [pcdata "Test Form:"];
        p [test_form ~state];
      ] in
      return (welcome)
    end

  let make ~state =
    (fun () () ->
      let main_title = "Test" in
      Template.default ~title:main_title
        (Authentication.authorizes (`view `test_service)
         >>= function
         | true ->
           test ~state >>= fun c ->
           return c
         | false ->
           Template.make_authentication_error ~main_title
             ~configuration:state.Hitscoreweb_state.configuration
             (return [Html5.pcdataf "You may not view the tests."])))
end

module Default_service = struct



  let make ~state =
    (fun () () ->
      let open Html5 in
      let content =
        Template.menu_ul ()
        >>= fun ul_menu ->
        let header = [h1 [pcdata "Gencore Home"];] in
        let menu =
          Option.value_map ul_menu ~default:[]
            ~f:(fun ul -> [h2 [pcdata "Menu"]; ul]) in
        let welcome = [
          h2 [pcdata "Welcome"];
          p [
            pcdata "This is Gencore's LIMS web-application; for general \
                    information see ";
            core_a
              ~a:[ a_hreff "http://biology.as.nyu.edu/object/biology.facilities.sequencing" ]
              [pcdata "Gencore's webpages"];
            pcdata ". ";
          ];
        ] in
        return (header @ welcome @ menu)
      in
      Template.default ~title:"Home" content)

end

module Doc_service = struct

  let sdx_to_html document =
    let open Sequme_doc_syntax in
    let open Html5 in
    let rec inline = function
      | Bold inside -> b (List.map inside inline)
      | Italic inside -> i (List.map inside inline)
      | Code  inside -> code (List.map inside inline)
      | Link (options, inside) ->
        begin match options with
        | [] -> span (List.map inside inline)
        | `url url :: _ when String.is_prefix url ~prefix:"mailto:" ->
          let id = unique_id "aid" in
          Template.anti_spam_mailto ~id ~mailto:url;
          span [core_a ~a:[a_id id; a_hreff "ah ah no-spam"]
                   (List.map inside inline)]
        | `url url :: _ ->
          span [core_a ~a:[a_hreff "%s" url] (List.map inside inline)]
        | `local l :: _ ->
          span [Html5.a ~fragment:l ~service:Eliom_service.void_coservice'
                   (List.map inside inline) ()]
        end
      | Text s ->
        pcdata s in
    let rec structural = function
      | Section (level, idopt, inside) ->
        let a = Option.value_map idopt ~default:[] ~f:(fun id -> [a_id id]) in
        begin match level with
        | `one   -> h1 ~a (List.map inside inline)
        | `two   -> h2 ~a (List.map inside inline)
        | `three -> h3 ~a (List.map inside inline)
        | `four  -> h4 ~a (List.map inside inline)
        end
      | Code_bloc (options, inside) -> pre (List.map inside inline)
      | Paragraph -> p []
      | Line_break -> br ()
      | Inline i ->  span (List.map i inline)
      | Numbered_list items ->
        ol (List.map items (function `item ls -> li (List.map ls structural)))
      | Unnumbered_list items ->
        ul (List.map items (function `item ls -> li (List.map ls structural)))
    in
    List.map document structural



  let make ~configuration =
    (fun path () ->
      let open Html5 in
      let content =
        begin match Configuration.root_path configuration with
        | Some rootpath ->
          read_file Filename.(
              (concat
                 (concat rootpath "doc")
                 (sprintf "%s.sdx" (String.concat path ~sep:"/"))))
        | None ->
          error `root_directory_not_configured
        end
        >>= fun file_content ->
        begin match Sequme_doc_syntax.parse ~pedantic:false file_content with
        | Ok document ->
          let subdocs = Sequme_doc_syntax.extract_level_one document in
          let divs =
            List.map subdocs (fun (_, title, content) ->
              let sec1 = Sequme_doc_syntax.Section (`one, None, title) in
              let (toc, doc) =
                Sequme_doc_syntax.table_of_contents ~toplevel:`two content in
              let header = sdx_to_html [sec1] in
              let tochtml =
                sdx_to_html [Sequme_doc_syntax.toc_to_numbered_list toc] in
              let content_html = sdx_to_html doc in
              div [div ~a:[a_class ["doc_doc"]] header;
                   div ~a:[ a_class ["doc_toc"]] tochtml;
                   div ~a:[a_class ["doc_doc"]] content_html])
          in
          return divs
        | Error _ ->
          return [pcdata "Error: could not parse content"]
        end
      in
      Template.default ~title:"Home" content)
end
module File_service = struct

  let people_of_libraries ~layout libs =
    while_sequential libs ~f:(fun sl ->
      layout#input_library#all
      >>| List.filter ~f:(fun il -> il#library#id = sl#g_id))
    >>| List.concat
    >>= while_sequential ~f:(fun il ->
      layout#lane#all
      >>| List.filter ~f:(fun lane ->
        Array.exists lane#libraries (fun l -> l#id = il#g_id)))
    >>| List.concat
    >>| List.map ~f:(fun lane ->
      Array.(to_list (map lane#contacts ~f:(fun c -> c#pointer))))
    >>| List.concat

  let indentify_and_verify ~configuration vol path =
    with_database ~configuration (fun ~dbh ->
      let vol_pointer = Layout.File_system.(unsafe_cast vol) in
      Common.all_paths_of_volume ~dbh ~configuration vol_pointer
      >>= fun all_paths ->
      let layout = Classy.make dbh in
      layout#file_system#get vol_pointer
      >>= fun vol_content ->
      begin match vol_content#g_kind with
      | `protocol_directory ->
        layout#protocol#all
        >>| List.filter ~f:(fun p -> p#doc#id = vol_content#g_id)
        >>= while_sequential ~f:(fun prot ->
          layout#stock_library#all
          >>| List.filter ~f:(fun sl ->
            match sl#protocol with None -> false | Some p -> p#id = prot#g_id))
        >>| List.concat
      | `bioanalyzer_directory  ->
        layout#bioanalyzer#all
        >>| List.filter ~f:(fun p ->
          match p#files with None -> false | Some f -> f#id = vol_content#g_id)
        >>= while_sequential ~f:(fun bio -> bio#library#get)
      | `agarose_gel_directory ->
        layout#agarose_gel#all
        >>| List.filter ~f:(fun p ->
          match p#files with None -> false | Some f -> f#id = vol_content#g_id)
        >>= while_sequential ~f:(fun ag -> ag#library#get)
      | k -> error (`forbidden_volume_kind k)
      end
      >>= fun stock_libraries ->
      people_of_libraries ~layout stock_libraries
      >>= fun people ->
      Authentication.authorizes (`view (`libraries_of people))
      >>= begin function
      | true -> return ()
      | false -> error (`wrong_access_rights)
      end
      >>= fun () ->
      let content_type =
        let mime_assoc = Ocsigen_charset_mime.default_mime_assoc () in
        Ocsigen_charset_mime.find_mime path mime_assoc in
      begin match
          List.mem all_paths path && Eliom_registration.File.check_file path
      with
      | true ->
        return (content_type, path)
      | false -> error (`path_not_right_volume path)
      end)

  let error_content e =
    let open Template in
    let open Html5 in
    return ( [Html5.pcdata "Error: Cannot retrieve that file …"])

  let make ~configuration =
    begin fun (vol, path) () ->
      let open Lwt  in
      indentify_and_verify ~configuration vol path
      >>= begin function
      | Ok (content_type, path) ->
        let headers =
          Http_headers.(
            add (name "Content-Disposition")
              (sprintf "attachment; filename=%s" (Filename.basename path))
              empty) in
        Eliom_registration.File.send ~headers ~content_type path
      | Error e ->
          (* Lwt.fail Eliom_common.Eliom_404 *)
        Template.default ~title:"File Error" (error_content e)
        >>= fun html ->
        Eliom_registration.Html5.send ~content_type:"text/html" html
      end
    end

end

let () =
  let open Lwt in
  Ocsigen_extensions.register_command_function ~prefix:"maintenance"
    (fun s c -> match c with
    | ["on"] ->
      log "Goin' maintenance mode\n" >>= fun _ ->
      Authentication.maintenance_mode_on ();
      return ()
    | ["off"] ->
      log "Leavin' maintenance mode\n" >>= fun _ ->
      Authentication.maintenance_mode_off ();
      return ()
    | _ -> raise Ocsigen_extensions.Unknown_command)

let () =
  Eliom_service.register_eliom_module
    "hitscoreweb"
    (fun () ->

      Lwt_preemptive.init 1 500 (eprintf "LwtP:%s\n%!");

      Sequme_flow_sys.Timeout.set_global_default 10.;

      let _ =
        (* From the doc: http://ocsigen.org/eliom/api/server/Eliom_output
           >   Note that you should not catch every exception here
           >   since some Eliom mechanisms are done using exceptions,
           >   like redirections. Do not catch exception defined in
           >   Eliom except Eliom_common.Eliom_404,
           >   Eliom_common.Eliom_Wrong_parameter
           >   Eliom_common.Eliom_Typing_Error.

           I don't understand why we see Eliom_404 exceptions
           everywhere, and only the `real' ones get redirected to the
           404. anyway, It Works™.
        *)
        let send ?code e =
          Lwt.bind (Template.default (error e))
            (Eliom_registration.Html5.send ?code) in
        Eliom_registration.set_exn_handler (function
          | Eliom_common.Eliom_404 -> send ~code:404 `eliom_404
          | Eliom_common.Eliom_Wrong_parameter -> send `eliom_wrong_parameter
          | Eliom_common.Eliom_Typing_Error l -> send (`eliom_typing_error l)
          (*
TODO: All exceptions in coservices should be handled in some other way
          *)
          | e ->
            eprintf "EXN: %s\n%s\n%!" (Exn.to_string e)
              (Exn.backtrace ()); raise e)
      in

      let state, hitscore_configuration, debug_mode =

        let pghost = ref None in
        let pgport = ref None in
        let pgdb = ref None in
        let pguser = ref None in
        let pgpass = ref None in
        let rodi = ref None in
        let vols = ref None in
        let raw = ref None in
        let hsd = ref None in
        let pam_service = ref None in
        let debug_mode = ref false in
        let open Simplexmlparser in
        let rec go_through = function
          | Element ("pghost", [], [PCData h]) -> pghost := Some h
          | Element ("pgport", [], [PCData p]) -> pgport := Some (int_of_string p)
          | Element ("pgdb", [], [PCData d]) -> pgdb := Some d
          | Element ("pguser", [], [PCData u]) -> pguser := Some u
          | Element ("pgpass", [], [PCData p]) -> pgpass := Some p
          | Element ("root-path", [], [PCData p]) -> rodi := Some p
          | Element ("vol-directory", [], [PCData p]) -> vols := Some p
          | Element ("raw-path", [], [PCData p]) -> raw := Some p
          | Element ("hiseq-dir", [], [PCData p]) -> hsd := Some p
          | Element ("debug", [], []) ->
            debug_mode := true;
          | Element ("pam-authentication-service", [], [PCData p]) ->
            pam_service := Some p;
          | Element (tag, atts, inside) ->
            Ocsigen_messages.console (fun () ->
              sprintf "Unknown Config XML Tag: %s\n" tag);
            List.iter inside go_through
          | PCData s -> ()
        in
        List.iter (Eliom_config.get_config ()) go_through;
        let db_configuration =
          match !pghost, !pgport, !pgdb, !pguser, !pgpass with
          | Some host, Some port, Some database, Some username, Some password ->
            Some (Configuration.db_configuration
                    ~host ~port ~database ~username ~password)
          | _ -> None
        in
        let config =
          Configuration.configure
            ?vol_directory:!vols ?raw_data_path:!raw ?hiseq_directory:!hsd
            ?root_path:!rodi ?db_configuration () in
        Authentication.init ~disabled:!debug_mode ?pam_service:!pam_service config;
        Web_data_access.init
          ~loop_time:(if !debug_mode then 90. else 600.)
          ~configuration:config ()
        |> Lwt.ignore_result;
        let state =
          Hitscoreweb_state.init_state ~configuration:config () in
        (state, config, debug_mode)
      in

      Services.(register default) (Default_service.make ~state);
      Services.(register home) (Default_service.make ~state);
      Services.(register test) (Test_service.make ~state);
      Services.(register submission_forms)
        (Hitscoreweb_submission_forms.make ~state);

      Services.(register hiseq_runs)
        Hitscoreweb_hiseq_runs.(make hitscore_configuration);

      Services.(register facility_statistics)
        Hitscoreweb_facility_stats.(make ~state);

      Services.(register persons) Hitscoreweb_persons.(make ~state);

      Services.(register libraries)
        Hitscoreweb_libraries.(
          make
            ~information_cache_timming:(
              if !debug_mode then (40., 60.) else (60., 1200.))
            ~configuration:hitscore_configuration);

      Services.(register flowcell)
        Hitscoreweb_flowcell_service.(make hitscore_configuration);

      Services.(register evaluations)
        Evaluations_service.(make ~configuration:hitscore_configuration);

      Layout_service.init_caml_service
        ~configuration:hitscore_configuration ();
      Services.(register layout)
        Layout_service.(make ~configuration:hitscore_configuration);

      Services.(register doc)
        Doc_service.(make ~configuration:hitscore_configuration);

      One_person_service.init_caml_service ~state ();
      One_person_service.init_email_verification_service ~state;
      Services.(register self) One_person_service.(make_self ~state);

      Services.(register person) One_person_service.(make_person ~state);

      Services.(register log) Hitscoreweb_log.(make ~state);

      Services.(register uploads) Hitscoreweb_uploads_service.(make ~state);
      Services.(register pgm_runs)
        Hitscoreweb_pgm_runs.(make ~state);

      Services.(register_css stylesheet)
        Template.(css_service_handler ~configuration:hitscore_configuration);

      Services.(register_file file)
        File_service.(make ~configuration:hitscore_configuration);

      Services.(register fastx_results)
        All_or_any_fastx_stats.(make ~configuration:hitscore_configuration);

      logf "All services are registered" |> Lwt.ignore_result;

    )
