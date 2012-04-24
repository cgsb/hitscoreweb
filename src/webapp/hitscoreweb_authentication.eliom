{shared{
open Hitscoreweb_std
}}

module Data_access = Hitscoreweb_data_access
  
module Services = Hitscoreweb_services

module Queries = Hitscoreweb_queries

type capability = [
| `view of [`all 
           | `person of Layout.Record_person.t
           | `all_evaluations
           | `all_flowcells
           | `all_hiseq_runs
           | `layout
           | `persons
           | `libraries
           | `libraries_of of Layout.Record_person.pointer list
           | `lane_of of Layout.Record_person.pointer list
           | `full_persons
           | `hiseq_raw_info
           | `demux_info
           | `flowcell]
| `edit of [
  | `password_of_person of Layout.Record_person.t
  | `names_of_person of Layout.Record_person.t
  | `layout]
| `impersonate of [`person of Layout.Record_person.t | `users]
]

let roles_allow ?(impersonation=false) ?person roles (cap:capability) =
  let module P = Layout.Record_person in
  let id_opt = Option.map person (fun p -> p.P.id) in
  let is_part_of_crew people =
    match person, people with
    | Some p, more :: than_zero ->
      List.exists people (fun x -> P.(x.id = p.id))
    | _ -> false
  in
  match cap with
  | `impersonate `users ->
    List.exists roles (fun c -> c = `auditor || c = `administrator)
  | `impersonate (`person pp)  ->
    List.exists roles (fun c -> c = `auditor || c = `administrator)
    && Array.for_all pp.Layout.Record_person.roles
      ~f:(fun r -> r <> `auditor && r <> `administrator)
  | `edit something when impersonation -> false
  | `edit (`names_of_person p) | `view (`person p) when id_opt = Some p.P.g_id -> true
  | `edit (`password_of_person p) when id_opt = Some p.P.g_id ->
    p.P.password_hash <> None
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
        | `persons | `libraries | `flowcell | `all_flowcells -> true
        | `libraries_of people when is_part_of_crew people -> true
        | `lane_of people when is_part_of_crew people -> true
        | _ -> false
      else
        false

type user_logged = {
  id: string;
  person: Layout.Record_person.pointer;
  roles: Layout.Enumeration_role.t list;
}
    
type authentication_state = [
| `nothing
| `user_logged of user_logged
| `user_impersonating of user_logged * user_logged
| `insufficient_credentials of string
| `error of string *
    [ `auth_state_exn of exn
    | `broker_not_initialized
    | `io_exn of exn
    | `login_not_found of string
    | `pam_exn of exn
    | `person_not_unique of string ]
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
  let prf fmt =
    ksprintf Ocsigen_messages.accesslog ("Authentication-state: " ^^ fmt) in
  begin match s with
  | `nothing -> prf "NOTHING"
  | `user_logged u -> prf "USER-LOGGED: %S" u.id
  | `insufficient_credentials i -> prf "INSUFFICIENT-CREDENTIALS: %S" i
  | `user_impersonating (a, u) -> prf "USER %S IMPERSONATING %S" a.id u.id
  | `error (id, e) ->
    prf "ERROR: %S -- %s" id
      (match e with
      | `auth_state_exn e
      | `io_exn e
      | `pam_exn e ->  (Exn.to_string e)
      | `broker_not_initialized -> "broker_not_initialized"
      | `login_not_found s -> sprintf "login_not_found: %S" s
      | `person_not_unique s -> sprintf "person_not_unique: %S" s)
  end;
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
  | `user_impersonating (_, u) :: _ -> return (Some u)
  | _ -> return None

let find_user login =
  Data_access.find_person_opt login
  >>= fun found ->
  begin match found with
  | Some p -> return p
  | None -> error (`login_not_found login)
  end
    
let make_user u = 
  let module P = Layout.Record_person in
  { id = u.P.email; roles = Array.to_list u.P.roles;
    person = P.unsafe_cast u.P.g_id}

let pam_auth ?service ~user ~password () =
  let service =
    Option.value ~default:!global_pam_service service in
  let wrap_pam f a = try Ok (f a) with e -> Error (`pam_exn e) in
  let auth () =
     wrap_pam (Simple_pam.authenticate service user) (password) 
  in
  Lwt_preemptive.detach auth () 

let hash_password person_id password =
  let open Cryptokit in
  let open Layout.Record_person in
  let to_hash = sprintf "gencore:%ld:%s" person_id password in
  transform_string (Hexa.encode ()) (hash_string (Hash.sha256 ()) to_hash)
  
let check = function
  | `user_password (identifier, password) ->
    let open Layout.Record_person in
    let checking_m =
      find_user identifier >>= fun person ->
      if person.password_hash = Some (hash_password person.g_id password)
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
      ~error:(fun e -> set_state (`error (identifier, e)))

let logout () =
  set_state `nothing

let authorizes (cap:capability) =
  get_state ()
  >>= fun state ->
  begin match state with
  | `user_logged u -> return (roles_allow ~person:u.person u.roles cap)
  | `user_impersonating (_, u) ->
    return (roles_allow ~impersonation:true ~person:u.person u.roles cap)
  | _ -> return !global_authentication_disabled
  end


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
          ~https:true
          ~post_params:Eliom_parameters.(string "user" ** string "pwd")
          (fun () (user, pwd) ->
            if Eliom_request_info.get_ssl ()
            then (check (`user_password (String.strip user,pwd))
                  >>= function
                  | Ok () -> return ()
                  | Error e -> return ())
            else return ())
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
            | Error e -> return ())
      in
      coserv := Some handler;
      handler

let start_impersonation_coservice = 
  let coserv = ref None in
  fun () ->
    match !coserv with
    | Some s -> s
    | None ->
      let check_and_set u =
        user_logged () >>= fun a ->
        of_option a (fun adminauditor ->
          find_user u >>= fun person ->
          authorizes (`impersonate (`person person)) >>= fun can ->
          if can
          then
            set_state (`user_impersonating (adminauditor, make_user person))
          else return ())
      in
      let handler =
        Eliom_output.Action.register_post_coservice'
          ~https:true
          ~post_params:Eliom_parameters.(string "user")
          (fun () (user) ->
            Lwt.bind
              (if Eliom_request_info.get_ssl ()
               then check_and_set (String.strip user)
               else return None)
              (fun _ -> Lwt.return ()))
      in
      coserv := Some handler;
      handler

let stop_impersonation_coservice = 
  let coserv = ref None in
  fun () ->
    match !coserv with
    | Some s -> s
    | None ->
      let reset_state () =
        get_state ()
        >>= fun state ->
        begin match state with
        | `user_impersonating (a, u) -> set_state (`user_logged a)
        | _ -> set_state (`nothing)
        end
      in
      let handler =
        Eliom_output.Action.register_post_coservice'
          ~https:true
          ~post_params:Eliom_parameters.unit
          (fun () () ->
            Lwt.bind (reset_state ())
              (fun _ -> Lwt.return ()))
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

let start_impersonating_form () =
  let open Html5 in
  Eliom_output.Html5.post_form ~service:(start_impersonation_coservice ())
    (fun (name) ->
      [
        pcdata "Impersonate someone else: ";
        Eliom_output.Html5.string_input ~input_type:`Text ~name ();
        Eliom_output.Html5.string_input
          ~input_type:`Submit ~value:"Start" ();
      ]) () 
  
let stop_impersonating_form () =
  let open Html5 in
  Eliom_output.Html5.post_form ~service:(stop_impersonation_coservice ())
    (fun () ->
      [span [
        Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Stop" ()
      ]])
  
let display_state ?in_progress_element () =
  let open Html5 in
  get_state () >>= fun s ->
  authorizes (`impersonate `users) >>= fun can_impersonate ->
  let state =
    match s with
    | `nothing -> pcdataf "No user"
    | `user_impersonating (a, u) ->
      span [
        pcdata "User: ";
        Eliom_output.Html5.a ~service:(Services.person ())
          [pcdataf "%s" a.id] (a.id, None);
        pcdataf " (%s) impersonating "
          (String.concat ~sep:", " (List.map a.roles 
                                      Layout.Enumeration_role.to_string));
        Eliom_output.Html5.a ~service:(Services.person ())
          [pcdataf "%s" u.id] (u.id, None);
        pcdataf " (%s) "
          (String.concat ~sep:", " (List.map u.roles 
                                      Layout.Enumeration_role.to_string));
      ]
    | `user_logged u -> 
      span [
        pcdata "User: ";
        Eliom_output.Html5.a ~service:(Services.self ()) [pcdataf "%s" u.id] None;
        pcdataf " (%s)"
          (String.concat ~sep:", " (List.map u.roles 
                                      Layout.Enumeration_role.to_string));
      ]
    | `error (s, _)
    | `insufficient_credentials s -> pcdataf "Wrong credentials for: %s" s
  in
  let impersonation_form =
    if can_impersonate then [pcdata "; "; start_impersonating_form ()] else [] in
  return (state
          :: (match s with
          | `user_logged _ -> 
            impersonation_form @ [pcdata "; "; logout_form () ()]
          | `user_impersonating _ -> 
            [stop_impersonating_form () ()] @ [pcdata "; "; logout_form () ()]
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
