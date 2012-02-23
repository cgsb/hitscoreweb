open Hitscoreweb_std

module Services = Hitscoreweb_services


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

let roles_allow ?person roles cap =
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
          | Some p -> List.exists people ((=) p)
          | None -> false
          end
        | _ -> false
      else
        false

type user_logged = {
  id: string;
  person: Layout.Record_person.pointer option;
  roles: Layout.Enumeration_role.t list;
}
    
type authentication_state = [
| `nothing
| `user_logged of user_logged
| `insufficient_credentials of string
| `wrong_pam of [ `pam_exn of exn]
| `error of [ 
  | `layout_inconsistency of
      [ `record_person ] * [ `select_did_not_return_one_tuple of string * int ]
  | `pg_exn of exn
  | `too_many_persons_with_same_login of 
      Layout.Record_person.pointer Core.Std.List.t 
  | `auth_state_exn of exn
  | `io_exn of exn
]
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

let validate_user u =  (* TODO *)
  let make_viewer id = return { id; roles = [`auditor]; person = None } in
  get_configuration () >>= fun configuration ->
  let normal_validation login =
    Hitscore_lwt.db_connect configuration >>= fun dbh ->
    Layout.Search.record_person_by_login ~dbh (Some login)
    >>= (function
    | [] -> error (`login_not_found login)
    | [one] -> return one
    | more -> error (`too_many_persons_with_same_login more))
    >>= fun found ->
    Layout.Record_person.(
      get ~dbh found >>= fun {email; roles; _ } ->
      Hitscore_lwt.db_disconnect configuration dbh >>= fun () ->
      return (email, Array.to_list roles))
    >>= fun (id, roles) ->
    return { id; roles; person = Some found}
  in
  match u with
  | `login id when id = "smondet" -> make_viewer "smondet@localhost"
  | `login id -> normal_validation id

let pam_auth ?service ~user ~password () =
  let service =
    Option.value ~default:!global_pam_service service in
  let wrap_pam f a = try Ok (f a) with e -> Error (`pam_exn e) in
  let auth () =
    let open Result in
    match wrap_pam (Simple_pam.authenticate service user) (password) with
    | Ok () -> Ok user
    | Error e -> Error e
  in
  let m =
    Lwt_preemptive.detach auth () 
    >>= fun name ->
    validate_user (`login name)
  in
  double_bind m
    ~ok:(fun user -> set_state (`user_logged user))
    ~error:(function
    | `pam_exn _ as e -> set_state (`wrong_pam e)
    | `email_no_allowed s
    | `login_not_found s -> set_state (`insufficient_credentials s)
    | `layout_inconsistency (`record_person,
                             `select_did_not_return_one_tuple (_, _)) 
    | `io_exn _
    | `auth_state_exn _
    | `pg_exn _ 
    | `too_many_persons_with_same_login _ as e -> set_state (`error e)
    )

let check = function
  | `pam (user, password) ->
    pam_auth ~user ~password ()

let logout () =
  set_state `nothing

let authorizes (cap:capability) =
  user_logged () >>= 
    begin function
    | Some u -> return (roles_allow ?person:u.person u.roles cap)
    | _ -> return !global_authentication_disabled
    end




exception Authentication_error of
    [ `auth_state_exn of exn | `io_exn of exn | `non_https_login ]

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
              (check (`pam (user,pwd))
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


let login_form () =
  let open Html5 in
  Eliom_output.Html5.post_form ~service:(login_coservice ())
    (fun (name, pwd) ->
      [span [pcdata "NetID: ";
          Eliom_output.Html5.string_input ~input_type:`Text ~name ();
          pcdata " Password: ";
          Eliom_output.Html5.string_input ~input_type:`Password ~name:pwd ();
          Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Login" ();
         ];
      ]) () 

let logout_form () =
  let open Html5 in
  Eliom_output.Html5.post_form ~service:(logout_coservice ())
    (fun () ->
      [span [
        Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Logout" ()
      ]])

let display_state () =
  let open Html5 in
  get_state ()
  >>= fun s ->
  let state =
    match s with
    | `nothing -> pcdataf "No user"
    | `user_logged u -> 
      span [
        pcdata "User: ";
        begin match u.person with
        | Some t -> 
          Services.(link persons) [codef "%s" u.id] (Some true, [u.id])
        | None -> codef "[%s]" u.id
        end;
        pcdataf " (%s)"
          (String.concat ~sep:", " (List.map u.roles 
                                      Layout.Enumeration_role.to_string));
      ]
    | `insufficient_credentials s -> pcdataf "Wrong credentials for: %s" s
    | `wrong_pam (`pam_exn e) ->
      pcdataf "Authentication failure";
    | `error e -> 
      begin match e with
      | `layout_inconsistency (`record_person, 
                               `select_did_not_return_one_tuple (s, i)) ->
        pcdataf "ERROR: LI: More than one tuple: %s, %d" s i
      | `auth_state_exn e | `io_exn e | `pg_exn e ->
        pcdataf "ERROR: I/O/PG: %s" (Exn.to_string e)
      | `too_many_persons_with_same_login tl ->
        pcdataf "ERROR: Found %d persons with same login" (List.length tl)
          
      end
  in
  return (state
          :: (match s with
          | `user_logged _ -> 
            [pcdata "; "; logout_form () ()]
          | _ -> 
             if Eliom_request_info.get_ssl () then
               [pcdata ". ";
                login_form ()]
             else
               [pcdata ": ";
                Eliom_output.Html5.a
                  ~service:Eliom_services.https_void_coservice'
                  [pcdata "Login with HTTPS"] ();
               pcdata "."]
             
          ))
