
{shared{
open Hitscoreweb_std
open Printf
}}

module Data_access = Hitscoreweb_data_access

module Queries = Hitscoreweb_queries

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

type one_time_post_coservice_error =
[ `auth_state_exn of exn
| `broker_error of
    [ `broker_not_initialized
    | `io_exn of exn
    | `layout_inconsistency of
        [ `File_system | `Function of string | `Record of string ] *
          [ `insert_cache_did_not_return_one_id of
                        string * int32 list
          | `insert_did_not_return_one_id of string * int32 list
                    | `select_did_not_return_one_tuple of string * int ]
    | `pg_exn of exn ]
| `io_exn of exn
| `pg_exn of exn
| `non_emptiness_violation
| `wrong_rights ]

let one_time_post_coservice_error =
  Eliom_references.eref ~secure:true
    ~scope:Eliom_common.client_process (None: one_time_post_coservice_error option)



    
{shared{
type up_message =
| Change_password of string * string * string  (* email, pwd1, pwd2 *)
deriving (Json)

type down_message = 
| Password_changed
| Error_string of string

let password_minimum_size = 8
}}

let caml_service =
  make_delayed (Eliom_services.service 
          ~path:["one_person_caml_service"]
          ~get_params:Eliom_parameters.(caml "param" Json.t<up_message>))

let reply ~configuration =
  function
  | Change_password (id, pw1, pw2) ->
    if pw1 <> pw2 then
      return (Error_string "Trying to mess around? passwords are not equal")
    else if String.length pw1 < password_minimum_size then
      return (Error_string "Trying to mess around? passwords are not long enough")
    else (
      let edition_m =
        Data_access.find_person id
        >>= fun person ->
        Authentication.authorizes (`edit (`password_of_person person))
        >>= fun can_edit ->
        if can_edit 
        then 
          let open Layout.Record_person in
          let new_hash =
            Authentication.hash_password person.g_id pw1 in
          let person = { person with password_hash = Some new_hash } in
          Hitscore_lwt.with_database ~configuration
            ~f:(Data_access.modify_person ~person)
           >>= fun () ->
          return Password_changed
        else
          return (Error_string "Trying to mess around? \
                    You do not have the right to modify this")
      in
      double_bind edition_m ~ok:return
        ~error:(function
        | e ->
          return (Error_string "Edition error … deeply sorry.")))
      
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
      Eliom_output.Caml.register ~service:(caml_service ())
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
  Eliom_services.onload {{
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
              let open Eliom_output.Html5 in
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
            let i_elt_1 = Eliom_client.Html5.of_input pw1 in
            let i_elt_2 = Eliom_client.Html5.of_input pw2 in
            let btn_elt = Eliom_client.Html5.of_button submit in
            let msg_elt =
              Eliom_client.Html5.of_element
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
                  | Password_changed ->
                    the_span##innerHTML <- Js.string "<b>Done.</b>";
                      return ()
                    | Error_string s ->
                      dbg "Got Error: %S" s;
                      the_span##innerHTML <- ksprintf Js.string "<b>Error: %s</b>" s;
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

    
let make_view_page ~home ~configuration person =
  let open Html5 in
  let open Template in
  wrap_io Eliom_references.get one_time_post_coservice_error
  >>= fun potential_error_to_display ->
  wrap_io (Eliom_references.set one_time_post_coservice_error) None
  >>= fun () ->
  Hitscore_lwt.with_database ~configuration ~f:(fun ~dbh ->
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
    let error_message =
      Option.value_map ~default:(span []) potential_error_to_display
        ~f:(fun e ->
          begin match e with
          | `auth_state_exn _
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
                html_of_error (e)))
          end)
    in
    let change_password_link =
      if not can_edit_password then []
      else change_password_interface person.Layout.Record_person.email
    in
    return (content_paragraph
              [error_message;
               ul [
                li (display_opt "Print Name" person.print_name);
                li (display "Given Name" person.given_name);
                li (display_opt "Middle Name" person.middle_name);
                li (display "Family Name" person.family_name);
                li (display_opt "Nick Name" person.nickname);
                li (display "Primary Email" person.email);
                li (display_array "Secondary Emails" person.secondary_emails);
                li (display_opt "Login/NetID" person.login);
                li (display "Authentication"
                      (match person.password_hash, person.login with
                      | None, None -> "Impossible"
                      | None, Some _ -> "NYU Only"
                      | Some _, None -> "Gencore password set (No NYU)"
                      | Some _, Some _ ->
                        "Gencore Passowrd set + NYU Available"));
               ];
               div edition_link;
               div change_password_link;
              ])
  )


let one_time_post_coservice ~redirection ~configuration person =
  Eliom_output.Redirection.register_post_coservice
    ~scope:Eliom_common.session
    ~max_use:1
    ~fallback:Services.(home ())
    ~post_params:Eliom_parameters.(
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
        if can_edit 
        then 
          if given_name <> "" && family_name <> ""
          then 
            let open Layout.Record_person in
            let print_name = if print_name = "" then None else Some print_name in
            let middle_name = if middle_name = "" then None else Some middle_name in
            let nickname = if nickname = "" then None else Some nickname in
            let person =
              { person with print_name; given_name;
                middle_name;
                family_name;
                nickname;
              } in
            Hitscore_lwt.with_database ~configuration
              ~f:(Data_access.modify_person ~person)
          else
            error `non_emptiness_violation
        else 
          error `wrong_rights
      in 
      Lwt.(
         modification >>= (function
         | Ok () ->
           return redirection
         | Error e ->
           Eliom_references.set one_time_post_coservice_error (Some e)
           >>= fun () ->
           return redirection)))

let make_edit_page ~home ~configuration person =
  let open Html5 in
  let open Template in
  Hitscore_lwt.with_database ~configuration ~f:(fun ~dbh ->
    let open Layout.Record_person in
    let not_nulls = ref [] in
    let not_null_id () =
      let id = unique_id "not_null" in not_nulls := id :: !not_nulls;
      id in
    let not_null_string_input_item ~item_name ~name ~value =
      let id = not_null_id () in
      span [
        pcdataf "%s: " item_name;
        Eliom_output.Html5.string_input
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
      Eliom_output.Html5.(
        post_form ~service:post_coservice
          (fun (print_name, (given_name, (middle_name, (family_name, nickname)))) ->
            [ul [
              li [pcdata "Print name: ";
                  string_input ~input_type:`Text ~name:print_name
                    ?value:person.print_name ();];
              li [not_null_string_input_item ~item_name:"Given Name"
                     ~name:given_name ~value:person.given_name];
              li [pcdata "Middle name: ";
                  string_input ~input_type:`Text ~name:middle_name
                    ?value:person.middle_name ();];
              li [not_null_string_input_item ~item_name:"Family name"
                     ~name:family_name ~value:person.family_name ];
              li [pcdata "Nickname: ";
                  string_input ~input_type:`Text ~name:nickname
                    ?value:person.nickname ();];
             ]; 
             Eliom_output.Html5.string_input
               ~a:[ a_id "edit_submit"] ~input_type:`Submit ~value:"Go" ();
             span ~a:[a_id "edit_error_message";
                      a_style "visibility: hidden; color: red; font-weight: bold;"]
               [pcdata "Cannot submit!"];
            ]) ())
    in

    Eliom_services.onload {{
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
          Data_access.find_person u.Authentication.id
          >>= fun person ->
          make_generic ~configuration person action
            ~home:(fun a () ->
              Eliom_services.preapply Services.(self ()) (Some a))
      end)
    
let make_person ~configuration =
  (fun (id, action) () ->
    Template.default ~title:"User Page"
      begin
        Data_access.find_person id
        >>= fun person ->
        make_generic ~configuration person action
          ~home:(fun a () ->
            Eliom_services.preapply Services.(person ()) (id, Some a))
      end)
      
