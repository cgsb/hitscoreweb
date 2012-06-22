
{shared{
open Hitscoreweb_std
open Printf
}}

let logf fmt = logf ("{one-person} " ^^ fmt)
  
module Data_access = Hitscoreweb_data_access

module Queries = Hitscoreweb_queries

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

type one_time_post_coservice_error =
[ `auth_state_exn of exn
| `broker_error of Data_access.broker_error
| `db_backend_error of Backend.error
| `io_exn of exn
| `pg_exn of exn
| `non_emptiness_violation
| `wrong_rights ]

let one_time_post_coservice_error =
  Eliom_reference.eref ~secure:true
    ~scope:Eliom_common.client_process (None: one_time_post_coservice_error option)


let email_verification_tokens =
  Eliom_reference.eref ~secure:true
    ~scope:Eliom_common.global ([]: (string * string * string) list)

let email_verification_service =
  make_delayed (Eliom_service.service
                  ~path:["email_verification"]
                  ~get_params:Eliom_parameter.(string "identifier"
                                                ** string "new_email"
                                                ** opt (string "old_email")
                                                ** string "key")) 

let init_email_verification_service ~configuration =
  let open Html5 in
  Output_app.register ~service:(email_verification_service ())
    (fun (id, (new_email, (old_email, key))) () ->
      let work_m =
        Data_access.find_person id
        >>= fun person ->
        Authentication.authorizes (`edit (`emails_of_person person))
        >>= fun can_edit ->
        if can_edit 
        then (
          wrap_io Eliom_reference.get email_verification_tokens
          >>= fun tokens ->
          begin match List.find tokens ~f:(fun (i, _, k) -> i = id && k = key) with
          | Some (_, next, _) ->
            let open Layout.Record_person in
            begin match old_email with
            | None ->
              let person =
                let g_value =
                  {person.g_value with
                    secondary_emails = Array.append [| new_email |]
                      person.g_value.secondary_emails } in
                { person with g_value } in
              with_database ~configuration (Data_access.modify_person ~person)
            | Some old ->
              if person.g_value.email = old then
                let person =
                  let g_value = {person.g_value with email = new_email} in
                  { person with g_value } in
                with_database ~configuration (Data_access.modify_person ~person)
              else 
                begin match Array.findi person.g_value.secondary_emails
                    (fun _ -> (=) old) with
                | Some (idx, _) ->
                  person.g_value.secondary_emails.(idx) <- new_email;
                  with_database ~configuration (Data_access.modify_person ~person)
                | None ->
                  error (`email_verification (`cannot_find_old_email old))
                end
            end
            >>= fun () ->
            let new_tokens =
              List.filter tokens ~f:(fun (i, _, k) -> not (i = id && k = key)) in
            wrap_io (Eliom_reference.set email_verification_tokens) new_tokens
            >>= fun () ->
            return [pcdata  "Your new email was successfully verified"]
          | None ->
            error (`email_verification (`cannot_find_user_key (id, key)))
          end)
        else
          Template.make_authentication_error ~configuration
            ~main_title:"Email Verification Page" 
            (return [Html5.pcdataf
                        "You should maybe login to complete this action."])
      in
      Template.default ~title:"Email Verification Page" work_m
    )
    

      
{shared{
type up_message =
| Change_password of string * string * string  (* email, pwd1, pwd2 *)
| Change_email of string * string
| Set_primary_email of string
| Delete_email of string
| Add_secondary_email of string * string
deriving (Json)

type down_message = 
| Success
| Email_verification_in_progress of int * string
| Error_string of string

let password_minimum_size = 8
}}

let caml_service =
  make_delayed (Eliom_service.service 
          ~path:["one_person_caml_service"]
          ~get_params:Eliom_parameter.(caml "param" Json.t<up_message>))

let do_edition id cap edition =
  let edition_m =
    Data_access.find_person id
    >>= fun person ->
    Authentication.authorizes (`edit (cap person))
    >>= fun can_edit ->
    if can_edit 
    then (edition person >>= fun () ->
          logf "Edition of %s successful" id >>= fun () ->
          return Success)
    else (
      logf "Attempt to modify %s with authorization" id
      >>= fun () ->
      return (Error_string "Trying to mess around? \
                    You do not have the right to modify this"))
  in
  let informationless_error = Error_string "Edition error … deeply sorry." in
  double_bind edition_m ~ok:return
    ~error:(function
    | `cannot_find_secondary_email ->
      logf "`cannot_find_secondary_email for %s" id >>= fun () ->
      return (Error_string "Did not find that secondary email")
    | `wrong_parameter s ->
      logf "`wrong_parameter for %s: %S" id s >>= fun () ->
      return (Error_string (sprintf "Wrong Parameter(s): %s" s))
    | `email_verification_in_progress (_, next) ->
      logf "`email_verification_in_progress for %s" id >>= fun () ->
      return (Email_verification_in_progress (40, next))
    | `io_exn e | `auth_state_exn e | `sendmail e ->
      logf "exn for %s: %s" id Exn.(to_string e) >>= fun () ->
      return (informationless_error)
    | `person_not_found p ->
      logf "Person %S not found (edition for %s)" p id
      >>= fun () ->
      return informationless_error
    | `person_not_unique p ->
      logf "Person %S not unique (edition for %s)" p id
      >>= fun () ->
      return informationless_error
    | `broker_not_initialized | `broker_error _ ->
      logf "Broker error (edition for %s)" id
      >>= fun () ->
      return informationless_error
    | `db_backend_error _ ->
      logf "DB-Backend error (edition for %s)" id
      >>= fun () ->
      return informationless_error
    )

let add_or_change_email ~id ?current ~next person =
  let key =
    let rng = Cryptokit.Random.pseudo_rng Time.(now () |! to_string) in
    let s = String.make 42 'B' in
    rng#random_bytes s 0 42;
    let b64 = Cryptokit.Base64.encode_compact () in
    b64#put_string s;
    b64#get_string in
  let service = 
    Eliom_service.preapply (email_verification_service ())
      (id, (next, (current, key))) in
  let uri = Html5.make_string_uri ~absolute:true ~service () in
  wrap_io Lwt.(fun () ->
    Eliom_reference.get email_verification_tokens
    >>= fun tokens ->
    Eliom_reference.set email_verification_tokens ((id, next, key) :: tokens)) ()
  >>= fun () ->
  send_mail ~sender:"gencore.bio@nyu.edu"
    ~subject:"Gencore Email Verification"
    ~reply_to:["noreply"]
    ~bcc:["sebastien.mondet@gmail.com"]
    ~recipients:[next ]
    (sprintf "Please, click on that link this link to verify \
                    your email address:\n\n%s\n\n" uri)
  >>= fun () ->
  error (`email_verification_in_progress (42, next))

    
let reply ~configuration =
  function
  | Change_password (id, pw1, pw2) ->
    do_edition id (fun p -> `password_of_person p) (fun person ->
      if pw1 <> pw2 then
        error (`wrong_parameter "Trying to mess around? passwords are not equal")
      else if String.length pw1 < password_minimum_size then
        error (`wrong_parameter
                  "Trying to mess around? passwords are not long enough")
      else 
        let open Layout.Record_person in
        let new_hash =
          Authentication.hash_password person.g_id pw1 in
        let person =
          let g_value = { person.g_value with password_hash = Some new_hash } in
          { person with g_value } in
        with_database ~configuration (Data_access.modify_person ~person))
  | Set_primary_email email ->
    do_edition email (fun p -> `emails_of_person p) (fun person ->
      let open Layout.Record_person in
      begin match Array.findi person.g_value.secondary_emails (fun _ -> (=) email) with
      | Some (idx, _) ->
        person.g_value.secondary_emails.(idx) <- person.g_value.email;
        let person =
          let g_value = { person.g_value with email = email } in
          { person with g_value } in
        with_database ~configuration (Data_access.modify_person ~person)
      | None -> error (`cannot_find_secondary_email)
      end)
  | Delete_email email ->
    do_edition email (fun p -> `emails_of_person p) (fun person ->
      let open Layout.Record_person in
      begin match Array.findi person.g_value.secondary_emails
          (fun _ -> (=) email) with
      | Some (idx, _) ->
        let secondary_emails =
          Array.filter person.g_value.secondary_emails ~f:((<>) email) in
        let person =
          let g_value = { person.g_value with secondary_emails } in
          { person with g_value } in
        with_database ~configuration (Data_access.modify_person ~person)
      | None -> error (`cannot_find_secondary_email)
      end)
  | Change_email (current, next) ->
    do_edition current (fun p -> `emails_of_person p)
      (add_or_change_email ~id:current ~current ~next)
  | Add_secondary_email (id, next) ->
    do_edition id (fun p -> `emails_of_person p)
      (add_or_change_email ~id ~next)
      
let init_caml_service ~configuration =
  let already = ref false in
  fun () ->
    if !already then () else (
      already := true;
      let fail fmt =
        ksprintf
          (fun s -> Lwt.fail (Failure ("wrong reply from server: " ^ s)))
          fmt
      in
      Eliom_registration.Ocaml.register ~service:(caml_service ())
        (fun param () ->
          Lwt.bind (reply ~configuration param) (function
          | Ok o -> Lwt.return (o : down_message)
          | Error e -> fail "unknown error")))
        
let change_password_interface person_email =
  let chgpwd_id = unique_id "change_password" in
  let the_link_like =
    let open Html5 in
    [span ~a:[a_id chgpwd_id; a_class ["like_link"]]
        [pcdata "You may change your GenCore password"]] in
  let caml = caml_service () in
  Eliom_service.onload {{
    let open Html5 in
    let open Lwt in
    try
      begin
        
        let call_caml msg =
          Eliom_client.call_caml_service ~service: %caml msg () in
        
        let the_span = get_element_exn %chgpwd_id in
        the_span##onclick <-
          Dom_html.(handler (fun ev ->
            the_span##onclick <- Dom_html.(handler (fun ev -> Js._true));
            the_span##innerHTML <-
              Js.string "Please, enter a <strong>good</strong> password twice: ";
            the_span##classList##remove(Js.string "like_link"); 
            let pw1, pw2, submit =
              let open Html5 in
              (string_input ~input_type:`Password (),
               string_input ~input_type:`Password (),
               button ~a:[a_style "visibility:hidden"]
                 ~button_type:`Button [pcdata "submit"]) in
            let check_handler in1 in2 subm msg =
              Dom_html.handler (fun _ ->
                let s1 = Js.to_string in1##value in
                let s2 = Js.to_string in2##value in
                if max (String.length s1) (String.length s2) < password_minimum_size
                then (
                  msg##innerHTML <-
                    ksprintf Js.string "Your password must be \
                                 <strong>at least %d characters long</strong>."
                    password_minimum_size;
                  subm##style##visibility <- Js.string "hidden";
                  msg##style##visibility <- Js.string "visible";
                ) else if s1 <> s2 then (
                  msg##innerHTML <-
                    Js.string "The two entries are <strong>not equal</strong>.";
                  subm##style##visibility <- Js.string "hidden";
                  msg##style##visibility <- Js.string "visible";
                ) else if password_minimum_size <= String.length s1 && s1 = s2
                  then (
                    subm##style##visibility <- Js.string "visible";
                    msg##style##visibility <- Js.string "hidden";
                    );
                  Js._true)
            in
            let i_elt_1 = Html5_to_dom.of_input pw1 in
            let i_elt_2 = Html5_to_dom.of_input pw2 in
            let btn_elt = Html5_to_dom.of_button submit in
            let msg_elt =
              Html5_to_dom.of_element
                (span ~a:[a_style " color: red; "] []) in
            i_elt_1##onchange  <- check_handler i_elt_1 i_elt_2 btn_elt msg_elt;
            i_elt_1##onkeyup   <- check_handler i_elt_1 i_elt_2 btn_elt msg_elt;
            i_elt_1##onmouseup <- check_handler i_elt_1 i_elt_2 btn_elt msg_elt;
            i_elt_2##onchange  <- check_handler i_elt_1 i_elt_2 btn_elt msg_elt;
            i_elt_2##onkeyup   <- check_handler i_elt_1 i_elt_2 btn_elt msg_elt;
            i_elt_2##onmouseup <- check_handler i_elt_1 i_elt_2 btn_elt msg_elt;
            btn_elt##onclick <- Dom_html.handler(fun _ ->
              the_span##innerHTML <- Js.string "<b>Processing …</b>";
              Lwt.ignore_result 
                begin
                  let s1 = Js.to_string i_elt_1##value in
                  let s2 = Js.to_string i_elt_2##value in
                  call_caml (Change_password ( %person_email, s1, s2))
                  >>= fun msg ->
                  begin match msg with
                  | Success ->
                    the_span##innerHTML <- Js.string "<b>Done.</b>";
                    reload ()
                  | Error_string s ->
                    dbg "Got Error: %S" s;
                    the_span##innerHTML <- ksprintf Js.string "<b>Error: %s</b>" s;
                    return ()
                  | Email_verification_in_progress _ ->
                    dbg "Wut?";
                    the_span##innerHTML <-
                      ksprintf Js.string
                      "<b>Error: Unexpected response from server</b>";
                    return ()
                  end
                end;
              Js._true
            );
              (* let form = Eliom_client.Html5.of_div (div [pw1; pw2; submit]) in *)
            Dom.appendChild the_span i_elt_1;
            Dom.appendChild the_span i_elt_2;
            Dom.appendChild the_span btn_elt;
            Dom.appendChild the_span msg_elt;
            Js._true
          ));
        
      end
    with e -> 
      dbg "Exception in onload for %S: %s" %chgpwd_id (Printexc.to_string e);
      ()
  }};
  the_link_like

    
let change_emails_interface person_email secondary_emails =
  let chgpwd_id = unique_id "change_emails" in
  let the_link_like =
    let open Html5 in
    [span ~a:[a_id chgpwd_id; a_class ["like_link"]]
        [pcdata "You may change your emails"]] in
  let caml = caml_service () in
  Eliom_service.onload {{
    let open Html5 in
    let open Lwt in
    try
      begin
        
        let call_caml msg =
          Eliom_client.call_caml_service ~service: %caml msg () in
        
        let change_email_button email =
          let span = 
            (span ~a:[a_class ["like_link"]] [pcdata "change"]) in
          let elt = Html5_to_dom.of_element span in
          elt##onclick <- Dom_html.(handler (fun ev ->
            dbg "change %s" email;
            elt##onclick <- Dom_html.(handler (fun ev -> Js._true)); 
            elt##innerHTML <- Js.string "change: ";
            elt##classList##remove(Js.string "like_link"); 
            elt##style##fontWeight <- Js.string "bold";
            let field, submit =
              let open Html5 in
              (string_input ~input_type:`Text ~value:email (),
               button ~button_type:`Button [pcdata "submit"]) in
            let submit_elt = Html5_to_dom.of_element submit in
            let field_elt = Html5_to_dom.of_input field in
            submit_elt##onclick <- Dom_html.(handler (fun ev ->
              elt##innerHTML <- Js.string "<b>In progress …</b>";
              Lwt.ignore_result 
                begin
                  let s = Js.to_string field_elt##value in
                  call_caml (Change_email (email, s))
                  >>= fun msg ->
                  begin match msg with
                  | Success ->
                    dbg "Wut?";
                    elt##innerHTML <-
                      ksprintf Js.string
                      "<b>Error: Unexpected response from server</b>";
                    return ()
                  | Email_verification_in_progress (t, s) ->
                    elt##innerHTML <-
                      ksprintf Js.string
                      "<b>An email has been sent to %S to verify the address \
                      (the link will expire in %d minutes).</b>" s t;
                    return ()
                  | Error_string s ->
                    dbg "Got Error: %S" s;
                    elt##innerHTML <- ksprintf Js.string "<b>Error: %s</b>" s;
                    return ()
                  end
                end;
              Js._true)); 
            Dom.appendChild elt field_elt;
            Dom.appendChild elt submit_elt;
            Js._true));
          span
        in

        let add_email_button person_email =
          let span = 
            (span ~a:[a_class ["like_link"]]
               [pcdata "add an email address"]) in
          let elt = Html5_to_dom.of_element span in
          elt##onclick <- Dom_html.(handler (fun ev ->
            dbg "add email";
            elt##onclick <- Dom_html.(handler (fun ev -> Js._true)); 
            elt##innerHTML <- Js.string "enter a valid email address: ";
            elt##classList##remove(Js.string "like_link"); 
            elt##style##fontWeight <- Js.string "bold";
            let field, submit =
              let open Html5 in
              (string_input ~input_type:`Text (),
               button ~button_type:`Button [pcdata "submit"]) in
            let submit_elt = Html5_to_dom.of_element submit in
            let field_elt = Html5_to_dom.of_input field in
            submit_elt##onclick <- Dom_html.(handler (fun ev ->
              elt##innerHTML <- Js.string "<b>In progress …</b>";
              Lwt.ignore_result 
                begin
                  let s = Js.to_string field_elt##value in
                  call_caml (Add_secondary_email (person_email, s))
                  >>= fun msg ->
                  begin match msg with
                  | Email_verification_in_progress (t, s) ->
                    elt##innerHTML <-
                      ksprintf Js.string
                      "<b>An email has been sent to %S to verify the address \
                      (the link will expire in %d minutes).</b>" s t;
                    return ()
                  | Error_string s ->
                    dbg "Got Error: %S" s;
                    elt##innerHTML <- ksprintf Js.string "<b>Error: %s</b>" s;
                    return ()
                  | Success ->
                    dbg "Wut?";
                    elt##innerHTML <-
                      ksprintf Js.string
                      "<b>Error: Unexpected response from server</b>";
                    return ()
                  end
                end;
              Js._true)); 
            Dom.appendChild elt field_elt;
            Dom.appendChild elt submit_elt;
            Js._true));
          span
        in
          
        let set_primary_email_button email =
          let span = 
            (span ~a:[a_class ["like_link"]] [pcdata "set as primary"]) in
          let elt = Html5_to_dom.of_element span in
          elt##onclick <- Dom_html.(handler (fun ev ->
            dbg "set_primary_email_button %s" email;
            elt##onclick <- Dom_html.(handler (fun ev -> Js._true)); 
            elt##innerHTML <- Js.string "<b>In progress …</b>";
            elt##classList##remove(Js.string "like_link"); 
            elt##style##fontWeight <- Js.string "bold";
            Lwt.ignore_result 
              begin
                call_caml (Set_primary_email email)
                >>= fun msg ->
                begin match msg with
                | Success ->
                  elt##innerHTML <-
                    ksprintf Js.string "<b>Done.</b>";
                  reload ()
                | Error_string s ->
                  dbg "Got Error: %S" s;
                  elt##innerHTML <- ksprintf Js.string "<b>Error: %s</b>" s;
                  return ()
                | Email_verification_in_progress _ ->
                  dbg "Wut?";
                  elt##innerHTML <- ksprintf Js.string
                    "<b>Error: Unexpected response from server</b>";
                  return ()
                end
              end;
            Js._true));
          span
        in
            
        let delete_email_button email =
          let span = 
            (span ~a:[a_class ["like_link"]] [pcdata "delete"]) in
          let elt = Html5_to_dom.of_element span in
          elt##onclick <- Dom_html.(handler (fun ev ->
            dbg "delete_email_button %s" email;
            elt##onclick <- Dom_html.(handler (fun ev -> Js._true)); 
            elt##innerHTML <- Js.string "<b>In progress …</b>";
            elt##classList##remove(Js.string "like_link"); 
            elt##style##fontWeight <- Js.string "bold";
            Lwt.ignore_result 
              begin
                call_caml (Delete_email email)
                >>= fun msg ->
                begin match msg with
                | Success ->
                  elt##innerHTML <-
                    ksprintf Js.string "<b>Done.</b>";
                  reload ()
                | Error_string s ->
                  dbg "Got Error: %S" s;
                  elt##innerHTML <- ksprintf Js.string "<b>Error: %s</b>" s;
                  return ()
                | Email_verification_in_progress _ ->
                  dbg "Wut?";
                  elt##innerHTML <- ksprintf Js.string
                    "<b>Error: Unexpected response from server</b>";
                  return ()
                end
              end;
            Js._true));
          span
        in

        let the_span = get_element_exn %chgpwd_id in
        the_span##onclick <-
          Dom_html.(handler (fun ev ->
            the_span##onclick <- Dom_html.(handler (fun ev -> Js._true));
            the_span##innerHTML <-
              Js.string "Please, configure your email addresses: ";
            the_span##classList##remove(Js.string "like_link"); 
            let to_attach =
              Html5_to_dom.of_div
                (div [ul
                         (li [pcdata %person_email;
                              pcdata " (primary) → ";
                              change_email_button %person_email;]
                          :: li [add_email_button %person_email;]
                          :: (List.map (fun e ->
                            li [pcdata e; pcdata " → ";
                                change_email_button e; pcdata ", ";
                                delete_email_button e; pcdata ", or ";
                                set_primary_email_button e])
                                (Array.to_list %secondary_emails)))
                     ]) in
            Dom.appendChild the_span to_attach;
            Js._true
          ));
        
      end
    with e -> 
      dbg "Exception in onload for %S: %s" %chgpwd_id (Printexc.to_string e);
      ()
  }};
  the_link_like

let make_view_page ~home ~configuration person =
  let open Html5 in
  let open Template in
  wrap_io Eliom_reference.get one_time_post_coservice_error
  >>= fun potential_error_to_display ->
  wrap_io (Eliom_reference.set one_time_post_coservice_error) None
  >>= fun () ->
  with_database ~configuration (fun ~dbh ->
    let open Layout.Record_person in
    let display title thing =
      [strong [pcdataf "%s: " title]; em [pcdataf "%s." thing]] in
    let display_opt title opt =
      display title (Option.value ~default:"N/A" opt) in
    let display_array title arr =
      display title
        (match arr with
        | [| |] -> "N/A"
        | s -> Array.to_list s |! String.concat ~sep:", ") in
    Authentication.authorizes (`edit (`names_of_person person))
    >>= fun can_edit_names ->
    begin if can_edit_names
      then 
        return [Template.a_link
                   (home "edit") [pcdata "You may edit names"] ();
                pcdata "."]
      else
        return [pcdata "You cannot edit your information."]
    end
    >>= fun edition_link ->
    Authentication.authorizes (`edit (`password_of_person person))
    >>= fun can_edit_password ->
    Authentication.authorizes (`edit (`emails_of_person person))
    >>= fun can_edit_emails ->
    let error_message =
      Option.value_map ~default:(span []) potential_error_to_display
        ~f:(fun e ->
          begin match e with
          | `auth_state_exn _
          | `db_backend_error _
          | `io_exn _
          | `pg_exn _
          | `non_emptiness_violation
          | `wrong_rights  as err ->
            Template.(error_span (
              pcdataf "There was an error while editing: " :: 
                html_of_error (err)))
          | `broker_error e ->
            Template.(error_span (
              pcdataf "There was an error while editing: " :: 
                html_of_error (`broker_error e)))
          end)
    in
    let change_password_link =
      if not can_edit_password then []
      else change_password_interface person.g_value.email in
    let change_emails_link =
      if not can_edit_emails then []
      else change_emails_interface
        person.g_value.email person.g_value.secondary_emails in
    let pv = person.g_value in
    return (content_paragraph
              [error_message;
               ul [
                li (display_opt "Print Name" pv.print_name);
                li (display "Given Name" pv.given_name);
                li (display_opt "Middle Name" pv.middle_name);
                li (display "Family Name" pv.family_name);
                li (display_opt "Nick Name" pv.nickname);
                li (display "Primary Email" pv.email);
                li (display_array "Secondary Emails" pv.secondary_emails);
                li (display_opt "Login/NetID" pv.login);
                li (display "Authentication"
                      (match pv.password_hash, pv.login with
                      | None, None -> "Impossible"
                      | None, Some _ -> "NYU Only"
                      | Some _, None -> "Gencore password set (No NYU)"
                      | Some _, Some _ ->
                        "Gencore Passowrd set + NYU Available"));
               ];
               div edition_link;
               div change_password_link;
               div change_emails_link;
              ])
  )


let one_time_post_coservice ~redirection ~configuration person =
  Eliom_registration.Redirection.register_post_coservice
    ~scope:Eliom_common.session
    ~max_use:1
    ~fallback:Services.(home ())
    ~post_params:Eliom_parameter.(
      string "print_name"
      ** string "given_name"
      ** string "middle_name"
      ** string "family_name"
      ** string "nickname"
    )
    (fun () (print_name, (given_name, (middle_name, (family_name, nickname)))) ->
      eprintf "%s %s\n%!" print_name given_name;
      let modification =
        Authentication.authorizes (`edit (`names_of_person person))
        >>= fun can_edit ->
        let open Layout.Record_person in
        if can_edit 
        then 
          if given_name <> "" && family_name <> ""
          then 
            let print_name = if print_name = "" then None else Some print_name in
            let middle_name = if middle_name = "" then None else Some middle_name in
            let nickname = if nickname = "" then None else Some nickname in
            let person =
              let g_value =
                { person.g_value with print_name; given_name;
                    middle_name; family_name; nickname; } in
              { person with g_value} in
            with_database ~configuration (Data_access.modify_person ~person)
             >>= fun () ->
            logf "Edition of %d (%s)'s names successful" person.g_id
              person.g_value.email
          else
            logf "Error in Edition of %d (%s)'s names: `non_emptiness_violation"
              person.g_id person.g_value.email
             >>= fun () ->
            error `non_emptiness_violation
        else 
            logf "Error in Edition of %d (%s)'s names: `wrong_rights"
              person.g_id person.g_value.email
             >>= fun () ->
          error `wrong_rights
      in 
      Lwt.(
         modification >>= (function
         | Ok () ->
           return redirection
         | Error e ->
           Eliom_reference.set one_time_post_coservice_error (Some e)
           >>= fun () ->
           return redirection)))

let make_edit_page ~home ~configuration person =
  let open Html5 in
  let open Template in
  with_database ~configuration (fun ~dbh ->
    let open Layout.Record_person in
    let not_nulls = ref [] in
    let not_null_id () =
      let id = unique_id "not_null" in not_nulls := id :: !not_nulls;
      id in
    let not_null_string_input_item ~item_name ~name ~value =
      let id = not_null_id () in
      span [
        pcdataf "%s: " item_name;
        Html5.string_input
          ~input_type:`Text ~a:[ a_id id; ] ~name ~value ();
        span ~a:[ a_id (id ^ "not_null_error_message");
                  a_style "visibility: hidden; color: red";
                ]
          [pcdataf "%s should not be empty!" item_name]
      ] in
       
    let form = 
      let post_coservice =
        one_time_post_coservice ~configuration
          ~redirection:(home "view" ()) person in
      Eliom_registration.Html5.(
        post_form ~service:post_coservice
          (fun (print_name, (given_name, (middle_name, (family_name, nickname)))) ->
            [ul [
              li [pcdata "Print name: ";
                  string_input ~input_type:`Text ~name:print_name
                    ?value:person.g_value.print_name ();];
              li [not_null_string_input_item ~item_name:"Given Name"
                     ~name:given_name ~value:person.g_value.given_name];
              li [pcdata "Middle name: ";
                  string_input ~input_type:`Text ~name:middle_name
                    ?value:person.g_value.middle_name ();];
              li [not_null_string_input_item ~item_name:"Family name"
                     ~name:family_name ~value:person.g_value.family_name ];
              li [pcdata "Nickname: ";
                  string_input ~input_type:`Text ~name:nickname
                    ?value:person.g_value.nickname ();];
             ]; 
             Html5.string_input
               ~a:[ a_id "edit_submit"] ~input_type:`Submit ~value:"Go" ();
             span ~a:[a_id "edit_error_message";
                      a_style "visibility: hidden; color: red; font-weight: bold;"]
               [pcdata "Cannot submit!"];
            ]) ())
    in

    Eliom_service.onload {{
      let open Dom_html in
      let hide_exn id =
        (get_element_exn id)##style##visibility <- Js.string "hidden"; in
      let show_exn id =
        (get_element_exn id)##style##visibility <- Js.string "visible" in
      let handle_change input_elt id ev =
        let contents = Js.to_string input_elt##value in
        if contents = ""
        then (
          hide_exn "edit_submit";
          show_exn "edit_error_message";
          show_exn (id ^ "not_null_error_message");
        ) else (
          hide_exn "edit_error_message";
          show_exn "edit_submit";
          hide_exn (id ^ "not_null_error_message");
        );
        Js._true
      in
      List.iter (fun id ->
        begin match opt_tagged (document##getElementById (Js.string id)) with
        | Some (Input input_elt) ->
          input_elt##onchange <- handler (handle_change input_elt id);
          input_elt##onkeyup <- handler (handle_change input_elt id);
          input_elt##onmouseup <- handler (handle_change input_elt id);
        | _ ->
          dbg "The id %S is not there or is not an <input>" id;
        end

      ) !( %not_nulls)
    }};

    let page = content_paragraph [form] in
    return page)
    
let make_generic ~home ~configuration person action = 
  begin match action with
  | Some "edit" -> 
    (Authentication.authorizes (`edit (`names_of_person person))
     >>= function
     | true ->
       Template.make_content ~configuration
         ~main_title:"Complete The Information About Yourself" 
         (make_edit_page ~home ~configuration person)
     | false ->
       Template.make_authentication_error ~configuration
         ~main_title:"User Page" 
         (return [Html5.pcdataf "You shall not edit anything there."]))
  | _ ->
    (Authentication.authorizes (`view (`person person))
     >>= function
     | true ->
       Template.make_content ~configuration ~main_title:"About Yourself" 
         (make_view_page ~home ~configuration person)
     | false ->
       Template.make_authentication_error ~configuration
         ~main_title:"User Page" 
         (return [Html5.pcdataf "You shall not view anything there."]))
  end

let make_self ~configuration =
  (fun action () ->
    Template.default ~title:"User Page"
      begin
        Authentication.user_logged ()
        >>= function
        | None ->
          (Template.make_authentication_error ~configuration
             ~main_title:"User Page" 
             (return [Html5.pcdataf "You shall not view anything there."]))
        | Some u ->
          Data_access.person_by_pointer u.Authentication.person
          >>= fun c ->
          Data_access.find_person Layout.Record_person.(c.g_value.email)
          >>= fun person ->
          make_generic ~configuration person action
            ~home:(fun a () ->
              Eliom_service.preapply Services.(self ()) (Some a))
      end)
    
let make_person ~configuration =
  (fun (id, action) () ->
    Template.default ~title:"User Page"
      begin
        Data_access.find_person id
        >>= fun person ->
        make_generic ~configuration person action
          ~home:(fun a () ->
            Eliom_service.preapply Services.(person ()) (id, Some a))
      end)
      
