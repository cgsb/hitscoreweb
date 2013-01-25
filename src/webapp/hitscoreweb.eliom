{shared{
open Hitscoreweb_std
}}

module Msg = Hitscoreweb_messages

module Web_data_access = Hitscoreweb_data_access

module Queries = Hitscoreweb_queries

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
          dbg "Modified form : %s"
            Deriving_Json.(to_string Json.t<Hitscoreweb_meta_form.form_content> modified_form);
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

  type user_submission_form = {
    persons_form: Hitscoreweb_meta_form.form option;
    libraries_form: Hitscoreweb_meta_form.form option;
    created: Time.t;
    last_modified: Time.t;
  }

  let _temporary_form_store = (ref [] : (int * int * user_submission_form) list ref)

  let forms_of_user id =
    List.filter_map !_temporary_form_store
      (fun (u, k, f) -> if u = id then Some (k, f) else None)
    |! return

  let find_form form_list ~key = List.Assoc.find form_list key

  let user_persons_form user_id key_opt = 
    match key_opt with
    | Some key ->
      forms_of_user user_id >>= fun forms ->
      begin match (find_form forms key) with
      | Some { persons_form  } -> return (persons_form)
      | _ -> return None
      end
    | None -> return None

  let user_libraries_form user_id key_opt = 
    match key_opt with
    | Some key ->
      forms_of_user user_id >>= fun forms ->
      begin match (find_form forms key) with
      | Some { libraries_form  } -> return (libraries_form)
      | _ -> return None
      end
    | None -> return None

  let save_forms user key_opt form_to_save  =
    match List.find !_temporary_form_store (fun (u, k, _) -> u = user && Some k = key_opt) with
    | Some (u, k, sub) ->
      dbg "Saving form %d for user: %d" k u;
      let to_save =
        match form_to_save with
        | `persons f ->
          { sub with persons_form = Some f; last_modified = Time.now () }
        | `libraries f ->
          { sub with libraries_form = Some f; last_modified = Time.now () }
      in
      _temporary_form_store :=
        List.map !_temporary_form_store (fun (uu, kk, f) ->
          if u = uu && k = kk then (u, k, to_save) else (uu, kk, f));
      return ()
    | None ->
      let new_key =
        (List.fold !_temporary_form_store ~init:0 ~f:(fun m (_,k,_) -> max m k))
        + 1 in
      let persons_form, libraries_form =
        match form_to_save with
        | `persons p -> Some p, None
        | `libraries l -> None, Some l in
      let to_save = { persons_form; libraries_form;
                      created = Time.now (); last_modified = Time.now () } in
      dbg "Adding submission %d (for persons) for user: %d" new_key user;
      _temporary_form_store := (user, new_key, to_save) :: !_temporary_form_store;
      return ()

  let save_persons_form user key_opt form =
    save_forms user key_opt (`persons form)
  let save_libraries_form user key_opt form =
    save_forms user key_opt (`libraries form)

  let delete_submission user key  =
    logf "Deleting submission %d of user %d" key user >>= fun () ->
    _temporary_form_store :=
      List.filter !_temporary_form_store (fun (u, k, _) ->
        not (u = user && k = key));
    return ()
      
  module Msg = struct

    open Hitscoreweb_meta_form

    let start_a_new_submission = [Markup.text "Start a new submission"]
    let edit_contacts = [Markup.text "Edit contacts"]
    let edit_libraries = [Markup.text "Edit libraries"]
    let delete_submission = [Markup.text "Delete this submission"]

    (* let TODO = [Markup.text "TODO"] *)
    let save = [Markup.text "Save"]
    let cancel = [Markup.text "Cancel"]
    let error s = [ksprintf Markup.text "UNEXPECTED ERROR: %s !!" s]

    let contacts_section = [Markup.text "Contacts"]
    let add_contact = [Markup.text "Add a contact"]
    let choose_contact = [Markup.text "Pick a user or create a new one:"]
    let create_new_user =  "Create a new user …"
    let given_name = "Given name (mandatory)"
    let middle_name = "Middle name"
    let family_name = "Family name (mandatory)"
    let net_id = "Net ID"
    let email = "Email address (mandatory)"

    let libraries_section = [Markup.text "Libraries"]
    let add_library = [Markup.text "Add a library"]
    let choose_or_create_library = 
      [Markup.text "Pick an existing library (of yours) or define a new one:"]
    let create_new_library = "Create a new library …"
    let library_name = "Library Name (mandatory)"

  end
    
  module Regexp = struct
    let mandatory_identifier =
      ("non-empty string of letters, numbers, underscores, and dashes",
       "[a-zA-Z0-9_-]+")
    let mandatory_string = ("non-empty string", ".+")
    let mandatory_email =
      ("valid email address", "[a-zA-Z0-9_-@\\.\\+]+")
    let optional_net_id = ("NYU Net ID", "[a-z0-9]*")

    let matches (_, rex) s =
      let t = Re_posix.re (sprintf "^%s$" rex) in
      let re = Re.compile t in
      Re.execp re s

    let check_simple_string r = function
      | `string_error s
      | `string s -> if matches r s then Ok s else Error (r, Some s)
      | _ -> Error (r, None)
        
  end


  let save_and_cancel, save_choice, cancel_choice =
    ([Msg.save; Msg.cancel], Some 0, Some 1)
    
  let contacts_form_key = "contacts"
  let new_contacts_form ~state =
    let open Hitscoreweb_meta_form in
    let open Form in
    Hitscoreweb_state.persons_info state
    >>= fun persons_info ->
    let all_contacts =
      List.map persons_info#persons (fun p ->
        (sprintf "%s, %s &lt;<code>%s</code>&gt;"
           p#t#family_name p#t#given_name p#t#email,
         string ~value:p#t#email ())) in
    let contacts_section =
      let make_model v = 
        let choice = Option.value ~default:"" v in
        let open Regexp in
        meta_enumeration 
          ~overall_question:Msg.choose_contact
          ~creation_cases:[
            (Msg.create_new_user, list [
              string ~text_question:Msg.given_name ~regexp:mandatory_string ();
              string ~text_question:Msg.middle_name ();
              string ~text_question:Msg.family_name ~regexp:mandatory_string ();
              string ~text_question:Msg.email ~regexp:mandatory_email ();
              string ~text_question:Msg.net_id ~regexp:optional_net_id ();
            ]);
          ]
          ~choice
            (("", empty) :: all_contacts)
        in
        section Msg.contacts_section [
          extensible_list ~question:Msg.add_contact
            ~model:(make_model None) []
        ] in
    return (make ~buttons:save_and_cancel
              ~key:contacts_form_key contacts_section)

  let validate_contacts f =
    let open Hitscoreweb_meta_form in
    let open Html5 in
    let errf smthelse =
      logf "validate_contacts: Simplification of form error: %s"
        (Simplification.to_string smthelse)
      >>= fun () ->
      error (`submission_form
                (`validate_contacts (Simplification.to_string smthelse))) in
    begin match Simplification.perform f.form_content with
    | `list contacts ->
      while_sequential contacts (function
      | `string known ->
        Hitscoreweb_data_access.find_person_opt known
        >>= begin function
        | Some _ -> return (`known known)
        | None -> return (`actually_not_known known)
        end
      | `list [given; middle; family; email; net_id] ->
        let open Regexp in
        let gn = check_simple_string mandatory_string given in
        let mn =
          match middle with `string "" -> None | `string s -> Some s | _ -> None in
        let fn = check_simple_string mandatory_string family in
        let em = check_simple_string mandatory_email email in
        let ni = check_simple_string optional_net_id net_id in
        let contact = (gn, mn, fn, em, ni) in
        begin match em with
        | Ok o -> Hitscoreweb_data_access.find_person_opt o
        | _ -> return None
        end
        >>= fun found_by_email ->
        begin match ni with
        | Ok o -> Hitscoreweb_data_access.find_person_opt o
        | _ -> return None
        end
        >>= fun found_by_net_id ->
        begin match found_by_email, found_by_net_id with
        | Some u, _ -> return (`redefined_known (contact, `found_by_email u))
        | _, Some u -> return (`redefined_known (contact, `found_by_net_id u))
        | _ -> return (`new_one contact)
        end
      | smthelse -> errf smthelse)
    | smthelse -> errf smthelse
    end

  let list_of_contacts_div f =
    let open Hitscoreweb_meta_form in
    let open Html5 in
    let error_span l = Template.error_span l in
    let email s = codef "%s" s in
    let display_tested_string ts how =
      match ts with
      | Ok o -> how o
      | Error ((msg, rex), None) ->
        error_span [pcdataf "Expecting: %s but got nothing" msg]
      | Error ((msg, rex), Some s) ->
        error_span [pcdataf "Expecting: %s but got " msg; ksprintf how "%S "s]
    in
    let optional_string s how =
      Option.value_map s ~default:(i [pcdata "<none>"]) ~f:how in
    let new_one (gn, mn, fn, em, ni) add =
      return [pcdata "New contact: ";
              ul [
                li [pcdata "Given name: ";  display_tested_string gn pcdata];
                li [pcdata "Middle name: "; optional_string mn pcdata];
                li [pcdata "Family name: ";  display_tested_string fn pcdata];
                li [pcdata "Email: ";  display_tested_string em email];
                li [pcdata "Net ID: ";  display_tested_string ni email];
              ];
              add] in
    validate_contacts f
    >>= fun validation_result ->
    while_sequential validation_result begin function
    | `known known -> return [email known]
    | `actually_not_known k ->
      return [error_span [pcdata "The contact "; email k;
                          pcdata " was not found … what are you trying to do?"]]
    | `new_one c -> new_one c (pcdata "")
    | `redefined_known ((gn, mn, fn, em, ni), `found_by_email u) ->
      new_one (gn, mn, fn, em, ni)
        (error_span [pcdata "There is already a user using that email: ";
                     display_tested_string em email])
    | `redefined_known ((gn, mn, fn, em, ni), `found_by_net_id u) ->
      new_one (gn, mn, fn, em, ni)
        (error_span [pcdata "There is already a user using that NetID: ";
                     display_tested_string ni email])
    end
    >>| List.map ~f:li
    >>= fun li_s ->
    return (div [pcdata "Contacts:"; ul li_s])


      
  let libraries_form_key = "libraries"
  let new_libraries_form ~state user_id =
    let open Hitscoreweb_meta_form in
    let open Form in
    Hitscoreweb_state.persons_info state
    >>= fun persons_info ->
    let all_libraries =
      List.find_map persons_info#persons (fun p ->
        if p#t#g_id = user_id
        then Some (List.map p#libraries (fun l ->
          let qn = 
            sprintf "%s%s"
              (Option.value_map l#project ~default:"" ~f:(sprintf "%s."))
              l#name in
          (sprintf "<code>%s</code>" qn, string ~value:qn ()))
          |! List.dedup |! List.sort ~cmp:compare)
        else None)
      |! Option.value ~default:[]
    in
    let libraries_section =
      let model =
        meta_enumeration 
          ~overall_question:Msg.choose_or_create_library
          ~creation_cases:[
            (Msg.create_new_library, list [
              string ~text_question:Msg.library_name
                ~regexp:Regexp.mandatory_identifier ();
            ]);
          ]
          ~choice:""
          (("", empty) :: all_libraries)
      in
      section Msg.libraries_section [
        extensible_list ~question:Msg.add_library ~model []
      ] in
    return (make ~buttons:save_and_cancel
              ~key:libraries_form_key libraries_section)

  let wrap_error m =
    m >>< begin function
    | Ok o -> return o
    | Error (_) ->
      error (Msg.error "STATE/DATABASE ERROR")
    end

  let submission_form ~state user_id form_key_opt =
    let open Hitscoreweb_meta_form in
    let open Form in
    let initial_action_buttons, initial_action_buttons_handler =
      let buttons =
        [Msg.edit_contacts; Msg.edit_libraries; Msg.delete_submission] in
      let handler ~when_contacts ~when_libraries ~when_delete choice =
        wrap_error
          begin match choice with
          | Some 0 -> when_contacts ()
          | Some 1 -> when_libraries ()
          | Some 2 -> when_delete ()
          | c ->
            logf "submission_form: Unexpected initial_action_button choice: \
                %s (user: %d, form-key: %s)"
              (Option.value_map ~default:"None" c ~f:(sprintf "%d"))
              user_id
              (Option.value_map ~default:"None" form_key_opt ~f:(sprintf "%d"))
            >>= fun () ->
            error (`string "wrong choice")
          end
      in
      (buttons, handler)
    in
    let start () =
      match form_key_opt with
      | None -> return (make ~buttons:[Msg.start_a_new_submission] empty)
      | Some _ ->
        return (make ~buttons:initial_action_buttons empty)
    in
    create ~state begin function
    | None -> start ()
    | Some ({form_content = Empty; _} as form) ->
      initial_action_buttons_handler form.form_choice
        ~when_contacts:(fun () ->
          user_persons_form user_id form_key_opt
          >>= begin function 
          | Some f -> return (`form f)
          | None -> new_contacts_form ~state
          end)
        ~when_libraries:(fun () ->
          user_libraries_form user_id form_key_opt
          >>= begin function 
          | Some f -> return (`form f)
            | None -> new_libraries_form ~state user_id
          end)
        ~when_delete:(fun () ->
          let key = Option.value_exn form_key_opt in
          delete_submission user_id key >>= fun () ->
          return reload)
    | Some form when
        form.form_choice = save_choice && form.form_key = Some contacts_form_key ->
      save_persons_form user_id form_key_opt form
      >>= fun () ->
      return reload
    | Some form when
        form.form_choice = save_choice && form.form_key = Some libraries_form_key ->
      save_libraries_form user_id form_key_opt form
      >>= fun () ->
      return reload
    | Some form when form.form_choice = cancel_choice -> start ()
    | _ -> 
      wrap_error (logf "submission_form: unexpected choice?")
      >>= fun () ->
      error (Msg.error "?")
    end

    
  let test_user = ref 0
  let pick_test_user_form ~state =
    let open Hitscoreweb_meta_form in
    let open Form in
    let contacts = ref [] in
    let simple_form reply =
      begin
        Hitscoreweb_state.persons_info state
        >>= fun persons_info ->
        let all_contacts =
          List.map persons_info#persons (fun p ->
            let choice =
              sprintf "%s, %s &lt;<code>%s</code>&gt;"
                p#t#family_name p#t#given_name p#t#email in
            contacts := (choice, p#t#g_id) :: !contacts;
            (choice, integer ~value:p#t#g_id ())) in
        return (make ~buttons:[ [Markup.text "Go"] ]
                  (section [Markup.text "Set the “test” user"] [
                    meta_enumeration ~choice:"" (("", empty) :: all_contacts);
                  ]))
      end
      >>< begin function
      | Ok o -> return o
      | Error (_) ->
        error (Msg.error "STATE/DATABASE ERROR")
      end
    in
    create ~state (function
    | None ->
      simple_form None
    | Some form ->
      begin match Simplification.perform form.form_content with
      | `integer new_user ->
        test_user := new_user;
        return reload
      | `empty ->
        test_user := 0;
        return reload
      | f ->
        wrap_error begin
          logf "pick_test_user_form: wrong form: %s" (Simplification.to_string f)
        end
        >>= fun () ->
        dbg " cannot parse form answer: %s" (Simplification.to_string f);
        error [Markup.text " cannot parse form answer"]
      end)
      
  let test ~state =
    let open Html5 in
    begin 
      forms_of_user !test_user
      >>= fun forms ->
      while_sequential forms ~f:(fun (k, f) ->
        map_option f.persons_form list_of_contacts_div
        >>| Option.value ~default:(div [pcdata "No contacts yet"])
        >>= fun contacts_display ->
        return (
          li [div [div [pcdataf "Created on %s, last modified on %s "
                           (Time.to_string f.created)
                           (Time.to_string f.last_modified)];
                   contacts_display;
                   submission_form ~state !test_user (Some k)]]))
      >>= fun forms_display ->
      let welcome = [
        h2 [pcdata "Welcome"];
        h3 [pcdata "Test Form:"];
        p [test_form ~state];
        h3 [pcdata "Submission Forms:"];
        p [pick_test_user_form ~state];
        p [pcdataf "Submission forms as %d:" !test_user];
        ul forms_display;
        p [submission_form ~state !test_user None];
      ] in
      return (welcome)
    end
    >>< begin function
    | Ok o -> return o
    | Error e ->
      let stringify fmt = ksprintf (fun s -> `string s) fmt in
      begin match e with
      | `submission_form (`validate_contacts s) ->
        error (stringify "Error while validating contacts: %S" s)
      | `io_exn e ->
        error (stringify "/test I/O error: %s" (Exn.to_string e))
      end
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
          read_file (rootpath ^/ "doc" ^/
                       (sprintf "%s.sdx" (String.concat path ~sep:"/")))
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
        Eliom_registration.File.send ~content_type path
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
        |! Lwt.ignore_result;
        let state = 
          Hitscoreweb_state.init_state ~configuration:config () in
        (state, config, debug_mode)
      in

      Services.(register default) (Default_service.make ~state);
      Services.(register home) (Default_service.make ~state);
      Services.(register test) (Test_service.make ~state);

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

      Services.(register_css stylesheet)
        Template.(css_service_handler ~configuration:hitscore_configuration);

      Services.(register_file file)
        File_service.(make ~configuration:hitscore_configuration);

      logf "All services are registered" |! Lwt.ignore_result;

    )

