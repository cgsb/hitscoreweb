(* Submission forms service *)
{shared{
  open Hitscoreweb_std
}}
module Authentication = Hitscoreweb_authentication
module Template = Hitscoreweb_template
module Services = Hitscoreweb_services

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
  let project_name = "Project Name (mandatory)"
  let library_short_description = "Short Description (optional)"
  let library_short_description_help =
    Markup.(par [
      text "Something to tell your collaborators/P.I. what's in there …"])
  let application = [Markup.text "Application (mandatory)"]
  let new_application =  "New application"
  let is_stranded = [Markup.text "Is the library stranded? (mandatory)"]
  let truseq_control = [Markup.text "TruSeq Control?"]
  let rna_seq_control = [Markup.text "RNA-Seq Control?"]
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
  let simple = Simplification.perform f.form_content in
  let errf smthelse =
    logf "validate_contacts: Simplification of form error:\n%S\nout of\n%S"
      (Simplification.to_string smthelse) (Simplification.to_string simple)
    >>= fun () ->
    error (`submission_form
              (`validate_contacts (Simplification.to_string smthelse))) in
  begin match simple with
  | `list contacts ->
    while_sequential contacts (function
    | `empty -> return `empty_contact
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
  let none = (i [pcdata "<none>"]) in
  let display_tested_string ?(optional=false) ts how =
    match ts with
    | Ok o -> how o
    | Error ((msg, rex), None) ->
      if optional then none
      else error_span [pcdataf "Expecting: %s but got nothing" msg]
    | Error ((msg, rex), Some s) ->
      error_span [pcdataf "Expecting: %s but got " msg; ksprintf how "%S "s]
  in
  let optional_string s how = Option.value_map s ~default:none ~f:how in
  let new_one (gn, mn, fn, em, ni) add =
    return [pcdata "New contact: ";
            ul [
              li [pcdata "Given name: ";  display_tested_string gn pcdata];
              li [pcdata "Middle name: "; optional_string mn pcdata];
              li [pcdata "Family name: ";  display_tested_string fn pcdata];
              li [pcdata "Email: ";  display_tested_string em email];
              li [pcdata "Net ID: ";
                  display_tested_string ~optional:true ni email];
            ];
            add] in
  validate_contacts f
  >>= fun validation_result ->
  while_sequential validation_result begin function
  | `empty_contact -> return [error_span [pcdata "Empty contact"]]
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


    
let standard_applications = [
  "Amplicon-Seq";
  "ChIP-seq";
  "DNA-seq";
  "IN-seq";
  "MNase-seq";
  "RNA-seq";
  "Re-sequencing";
  "de-novo";
]
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
  let all_applications =
    ("", empty) ::
      List.map standard_applications ~f:(fun s -> (s, string ~value:s ())) in
  let libraries_section =
    let model =
      meta_enumeration 
          ~overall_question:Msg.choose_or_create_library
          ~creation_cases:[
            (Msg.create_new_library, list [
              string ~text_question:Msg.library_name
                ~regexp:Regexp.mandatory_identifier ();
              string ~text_question:Msg.project_name
                ~regexp:Regexp.mandatory_identifier ();
              string ~text_question:Msg.library_short_description
                ~help:Msg.library_short_description_help ();
              meta_enumeration ~overall_question:Msg.application
                ~creation_cases:[Msg.new_application,
                                 string ~regexp:Regexp.mandatory_string ();]
                ~choice:""
                all_applications;
              string_enumeration ~question:Msg.is_stranded
                ~value:"" [""; "Yes"; "No"];
              string_enumeration ~question:Msg.truseq_control
                ~value:"" [""; "Yes"; "No"];
              string ~question:Msg.rna_seq_control ();
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
 
let all_submissions ~state ~can_edit_own ~can_edit_all =
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
                 begin if can_edit_all
                   then submission_form ~state !test_user (Some k)
                   else div [strongf "(cannot edit)"]
                 end;
                ]]))
    >>= fun forms_display ->
    let welcome = [
      h2 [pcdata "Welcome"];
      h3 [pcdata "Submission Forms:"];
      p [pick_test_user_form ~state];
      p [pcdataf "Submission forms as %d:" !test_user];
      ul forms_display;
      begin if can_edit_all
        then p [submission_form ~state !test_user None]
        else p [strongf "(cannot create a new form)"]
      end;
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
    
let own_submissions ~state ~can_edit =
  error (`string "NOT IMPLEMENTED")

let dispatch ~state =
  Authentication.authorizes (`view `submission_forms)
  >>= fun can_view_own ->
  Authentication.authorizes (`view `all_submission_forms)
  >>= fun can_view_all ->
  Authentication.authorizes (`edit `own_submission_forms)
  >>= fun can_edit_own ->
  Authentication.authorizes (`edit `all_submission_forms)
  >>= fun can_edit_all ->
  begin match can_view_all with
  | true -> all_submissions ~state ~can_edit_all ~can_edit_own
  | false -> own_submissions ~state ~can_edit:can_edit_own
  end
  
let make ~state =
  (fun () () ->
    let main_title = "Submission Forms" in
    Template.default ~title:main_title
      (Authentication.authorizes (`view `submission_forms)
       >>= function
       | true ->
         dispatch ~state >>= fun c ->
         return c
       | false ->
         Template.make_authentication_error ~main_title
           ~configuration:state.Hitscoreweb_state.configuration
           (return [Html5.pcdataf "You may not view the submission forms."])))
