
{shared{
open Hitscoreweb_std
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
    Authentication.authorizes (`edit (`person person))
    >>= fun can_edit ->
    begin if can_edit
      then 
        return [Template.a_link
                   (home "edit") [pcdata "You may edit part of this"] ();
                pcdata "."]
      else
        return [pcdata "You cannot edit your information."]
    end
    >>= fun edition_link ->
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
               div edition_link
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
        Authentication.authorizes (`edit (`person person))
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
    (Authentication.authorizes (`edit (`person person))
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
      
