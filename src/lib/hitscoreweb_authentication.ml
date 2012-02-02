open Hitscoreweb_std

module Services = Hitscoreweb_services


type capability = [
| `view of [`all | `all_flowcells | `persons 
           | `full_persons | `full_flowcell]
]

let roles_allow roles cap =
  match cap with
  | `view smth ->
    List.exists roles (fun c -> c = `auditor || c = `administrator)
    || (List.exists roles ((=) `user) && smth = `persons)

type user_logged = {
  id: string;
  person: Layout.Record_person.t option;
  roles: Layout.Enumeration_role.t list;
}
    
type authentication_state = [
| `nothing
| `user_logged of user_logged
| `insufficient_credentials of string
| `wrong_pam of [`wrong_login| `pam_exn of exn]
| `error of [ 
  | `layout_inconsistency of
      [ `record_person ] * [ `select_did_not_return_one_cache of string * int ]
  | `pg_exn of exn
  | `too_many_persons_with_same_login of Layout.Record_person.t Core.Std.List.t ]
]

let authentication_history =
  Eliom_references.eref 
    ~scope:Eliom_common.session ([]: authentication_state list)
     
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

let validate_user  ~configuration u =  (* TODO *)
  let make_viewer id = return { id; roles = [`auditor]; person = None } in
  let normal_validation login =
    Hitscore_lwt.db_connect configuration >>= fun dbh ->
    Layout.Search.record_person_by_login ~dbh (Some login)
    >>= (function
    | [] -> error (`login_not_found login)
    | [one] -> return one
    | more -> error (`too_many_persons_with_same_login more))
    >>= fun found ->
    Layout.Record_person.(
      cache_value ~dbh found >>| get_fields
      >>= fun {email; roles; _ } ->
      Hitscore_lwt.db_disconnect configuration dbh >>= fun () ->
      return (email, Array.to_list roles))
    >>= fun (id, roles) ->
    return { id; roles; person = Some found}
  in
  match u with
  | `login id when id = "smondet" -> make_viewer "smondet@localhost"
  | `login id -> normal_validation id


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
    | `wrong_pam wp ->
      pcdataf "PAM: %s"
        (match wp with
        | `wrong_login -> "wrong login or password"
        | `pam_exn e -> sprintf "error: %s" (Exn.to_string e))
    | `error e -> 
      begin match e with
      | `layout_inconsistency (`record_person, 
                               `select_did_not_return_one_cache (s, i)) ->
        pcdataf "ERROR: LI: More than one cache: %s, %d" s i
      | `pg_exn e ->
        pcdataf "ERROR: PG: %s" (Exn.to_string e)
      | `too_many_persons_with_same_login tl ->
        pcdataf "ERROR: Found %d persons with same login" (List.length tl)
      end
  in
  return (state
          :: (match s with
          | `user_logged _ -> 
            [pcdata "; ";
             Services.(link logout) [pcdata "Logout"] ();]
          | _ -> 
            [pcdata "; ";
             Services.(link login) [pcdata "Login"] ();]
          ))


let pam_auth ~configuration ?(service = "login") ~user ~password () =
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
    validate_user ~configuration (`login name)
  in
  double_bind m
    ~ok:(fun user -> set_state (`user_logged user))
    ~error:(function
    | `pam_exn _ as e -> set_state (`wrong_pam e)
    | `email_no_allowed s
    | `login_not_found s -> set_state (`insufficient_credentials s)
    | `layout_inconsistency (`record_person,
                             `select_did_not_return_one_cache (_, _)) 
    | `pg_exn _ 
    | `too_many_persons_with_same_login _ as e -> set_state (`error e)
    )


let check ~configuration = function
  | `pam (user, password) ->
    pam_auth ~configuration ~user ~password ()

let logout () =
  set_state `nothing

let authorizes (cap:capability) =
  user_logged () >>= 
    begin function
    | Some u -> return (roles_allow u.roles cap)
    | _ -> return false
    end
