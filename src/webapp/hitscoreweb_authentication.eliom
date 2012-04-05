{shared{
open Hitscoreweb_std
}}

module Services = Hitscoreweb_services

module Queries = Hitscoreweb_queries

type capability = [
| `view of [`all 
           | `all_evaluations
           | `all_flowcells
           | `layout
           | `persons
           | `libraries
           | `libraries_of of Layout.Record_person.pointer list
           | `full_persons
           | `full_flowcell]
| `edit of [`layout]
]

let roles_allow ?person roles (cap:capability) =
  match cap with
  | `edit something ->
    if List.exists roles (fun c -> c = `administrator) then
      true
    else
      false
  | `view something ->
    if List.exists roles (fun c -> c = `auditor || c = `administrator) then
      true
    else
      let is_user = List.exists roles ((=) `user)  in
      if is_user then
        match something with
        | `persons | `libraries -> true
        | `libraries_of [] -> false
        | `libraries_of people ->
          begin match person with
          | Some p ->
            List.exists people (fun x -> Layout.Record_person.(x.id = p.g_id))
          | None -> false
          end
        | _ -> false
      else
        false

type user_logged = {
  id: string;
  person: Layout.Record_person.t;
  roles: Layout.Enumeration_role.t list;
}
    
type authentication_state = [
| `nothing
| `user_logged of user_logged
| `insufficient_credentials of string
]

let authentication_history =
  Eliom_references.eref ~secure:true
    ~scope:Eliom_common.session ([]: authentication_state list)
   
let authentication_configuration = 
  ref (None: Configuration.local_configuration option)

let global_authentication_disabled = ref false
let global_pam_service = ref ("")  

let init ?(disabled=false) ?(pam_service="") configuration  =
  authentication_configuration := (Some configuration);
  global_pam_service := pam_service;
  global_authentication_disabled := disabled

let get_configuration () =
  match !authentication_configuration with
  | Some e -> return e
  | None -> error (`auth_state_exn (Failure "Not initialized"))

  
let set_state s =
  let on_exn e = `auth_state_exn e in
  wrap_io ~on_exn Eliom_references.get authentication_history
  >>= fun ah ->
  wrap_io ~on_exn (Eliom_references.set authentication_history) (s :: ah)

let get_state () =
  let on_exn e = `auth_state_exn e in
  wrap_io ~on_exn Eliom_references.get authentication_history
  >>= function
  | [] | `nothing :: _ -> return `nothing
  | h :: t -> return h

let user_logged () =
  wrap_io Eliom_references.get authentication_history
  >>= function
  | `user_logged u :: _ -> return (Some u)
  | _ -> return None

let find_user login =
  get_configuration () >>= fun configuration ->
  Hitscore_lwt.with_database configuration (fun ~dbh ->
    Queries.person_of_any_identifier ~dbh login
    >>= (function
    | [] -> error (`login_not_found login)
    | [one] -> return one
    | more -> error (`too_many_persons_with_same_login more))
    >>= fun found ->
    Layout.Record_person.(get ~dbh found))
      
let make_user u = 
  let module P = Layout.Record_person in
  { id = u.P.email; roles = Array.to_list u.P.roles; person = u}

let pam_auth ?service ~user ~password () =
  let service =
    Option.value ~default:!global_pam_service service in
  let wrap_pam f a = try Ok (f a) with e -> Error (`pam_exn e) in
  let auth () =
     wrap_pam (Simple_pam.authenticate service user) (password) 
  in
  Lwt_preemptive.detach auth () 
      
let check = function
  | `user_password (identifier, password) ->
    let open Layout.Record_person in
    let checking_m =
      find_user identifier >>= fun person ->
      if person.password_hash = Some Digest.(string password |! to_hex)
      then
        set_state (`user_logged (make_user person))
      else
        begin
          of_option person.login (fun user -> pam_auth ~user ~password ())
          >>= fun pammed ->
          if pammed = Some ()
          then
            set_state (`user_logged (make_user person))
          else 
            set_state (`insufficient_credentials identifier)
        end
    in
    double_bind checking_m
      ~ok:return
      ~error:(fun e -> set_state (`insufficient_credentials identifier))

let logout () =
  set_state `nothing

let authorizes (cap:capability) =
  user_logged () >>= 
    begin function
    | Some u -> return (roles_allow ~person:u.person u.roles cap)
    | _ -> return !global_authentication_disabled
    end




exception Authentication_error of
    [ `auth_state_exn of exn
    | `io_exn of exn
    | `pg_exn of exn
    | `layout_inconsistency of
        [ `Record of string ] *
          [ `insert_did_not_return_one_id of
              string * int32 Hitscoreweb_std.List.container
          | `more_than_one_flowcell_called of string
          | `more_than_one_person_with_that_email
          | `no_last_modified_timestamp of int32
          | `select_did_not_return_one_tuple of string * int ]
    | `non_https_login ]

let login_coservice = 
  let coserv = ref None in
  fun () ->
    match !coserv with
    | Some s -> s
    | None ->
      let open Lwt in
      let pam_handler =
        (* This coservice is created/registered once, and then re-used
           for every login.
           The function Authentication.check handles the
           session-dependent stuff. *)
        Eliom_output.Action.register_post_coservice'
          (* ~fallback:Services.(home ()) *)
          (* ~https:true  --> Forces HTTPS *)
          ~post_params:Eliom_parameters.(string "user" ** string "pwd")
          (fun () (user, pwd) ->
            if Eliom_request_info.get_ssl () then
              (check (`user_password (user,pwd))
               >>= function
               | Ok () ->
                 return ()
               | Error e ->
                 Lwt.fail (Authentication_error e))
            else
              (Lwt.fail (Authentication_error `non_https_login)))
      in
      coserv := Some pam_handler;
      pam_handler

let logout_coservice =
  let coserv = ref None in
  fun () ->
    match !coserv with
    | Some s -> s
    | None ->
      let open Lwt in
      let handler =
        Eliom_output.Action.register_post_coservice'
          ~post_params:Eliom_parameters.unit
          (fun () () -> 
            logout () >>= function
            | Ok () -> return ()
            | Error e ->
              Lwt.fail (Authentication_error e))
      in
      coserv := Some handler;
      handler


let login_form ?set_visibility_to () =
  let open Html5 in
  let form_span_id = "span_login_form" in
  let message_span_id = "span_login_message" in
  Eliom_output.Html5.post_form ~service:(login_coservice ())
    (fun (name, pwd) ->
      [
        span ~a:[ a_id message_span_id; a_style "visibility: hidden" ] [];
        span ~a:[ a_id form_span_id;]
          [pcdata "NetID: ";
          Eliom_output.Html5.string_input ~input_type:`Text ~name ();
          pcdata " Password: ";
          Eliom_output.Html5.string_input ~input_type:`Password ~name:pwd ();
          Eliom_output.Html5.string_input
            ~a:[
              (* The onclick seems to work also when the user types <enter>
                 but onsubmit does not seem to to anything (?)
                 a_onsubmit {{debugf %dbgsrv "onsubmit!"}}; *)
              a_onclick {{
                let form_span =
                    Dom_html.document##getElementById (Js.string %form_span_id) in
                let message_span =
                    Dom_html.document##getElementById (Js.string %message_span_id) in
                Js.Opt.iter form_span (fun span ->
                  span##style##visibility  <- Js.string "hidden";);
                Js.Opt.iter message_span (fun span ->
                  span##style##visibility  <- Js.string "visible";
                  span##innerHTML <- Js.string "<b>Processing …</b>";);
                begin match %set_visibility_to with
                | Some s ->
                  (get_element_exn s)##style##visibility <- Js.string "visible";
                | None -> ()
                end
              }};
            ]
            ~input_type:`Submit ~value:"Login" ();
         ];
      ]) () 

let logout_form () =
  let open Html5 in
  Eliom_output.Html5.post_form ~service:(logout_coservice ())
    (fun () ->
      [span [
        Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Logout" ()
      ]])

let display_state ?in_progress_element () =
  let open Html5 in
  get_state ()
  >>= fun s ->
  let state =
    match s with
    | `nothing -> pcdataf "No user"
    | `user_logged u -> 
      span [
        pcdata "User: ";
        Eliom_output.Html5.a ~service:(Services.persons ())
          [pcdataf "%s" u.id] (Some true, [u.id]);
        pcdataf " (%s)"
          (String.concat ~sep:", " (List.map u.roles 
                                      Layout.Enumeration_role.to_string));
      ]
    | `insufficient_credentials s -> pcdataf "Wrong credentials for: %s" s
  in
  return (state
          :: (match s with
          | `user_logged _ -> 
            [pcdata "; "; logout_form () ()]
          | _ -> 
             if Eliom_request_info.get_ssl () then
               [pcdata ". ";
                login_form ?set_visibility_to:in_progress_element ()]
             else
               [pcdata ": ";
                Eliom_output.Html5.a
                  ~service:Eliom_services.https_void_coservice'
                  [pcdata "Login with HTTPS"] ();
               pcdata "."]
             
          ))
