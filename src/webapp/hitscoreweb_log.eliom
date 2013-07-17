(* The /log service implementation. *)

open Hitscoreweb_std_server
{shared{
open Hitscoreweb_std
}}
module Web_data_access = Hitscoreweb_data_access
module Authentication = Hitscoreweb_authentication
module Template = Hitscoreweb_template

let display_output_box content =
  let open Html5 in
  let box =
    div ~a:[
      a_style "border: 1px solid black; max-height: 40em; overflow: auto"
    ] [
      pre [pcdata content];
    ] in
  ignore {unit{
    let elt = Html5_to_dom.of_div %box in
    elt##scrollTop <- elt##scrollHeight;
  }};
  box

let user_spying () =
  let open Html5 in
  let user_string id =
    Web_data_access.get_person_by_id id
    >>= begin function
    | Some p ->
      return (sprintf "%d (%s, %s)" p#t#g_id p#t#family_name p#t#given_name)
    | None -> return "NO-USER"
    end
  in
  Map.fold !Authentication.global_user_spying_log
    ~init:(return [])
    ~f:(fun ~key ~data acc ->
      acc >>= fun acc ->
      user_string key
      >>= fun name ->
      let section = div [h3 [pcdata name]] in
      while_sequential data (fun (imp, timestamp, log) ->
        map_option imp user_string
        >>| Option.value_map ~default:(span []) ~f:(fun s -> strong [pcdataf "[as %s]" s])
        >>= fun impersonation ->
        return (li [
          codef "[%s]" Time.(to_string timestamp);
          impersonation;
          pcdataf ": %s" log
        ]))
      >>= fun content ->
      return (div [section; div [ul content]] :: acc))


let make ~state =
  (fun () () ->
    let open Html5 in
    let content =
      Authentication.authorizes (`view `log)
      >>= begin function
      | true ->
        Authentication.spy_userf "Visit /log" >>= fun () ->
        Sequme_flow_sys.get_system_command_output
          (sprintf "tail -n 2000 %s" (log_file_path ()))
        >>= fun (stdout, stderr) ->
        with_database state.Hitscoreweb_state.configuration (fun ~dbh ->
          let layout = Classy.make dbh in
          layout#log#all >>= fun logs ->
          while_sequential logs (fun log -> return (log#g_created, log#log)))
        >>| List.sort ~cmp:(fun a b -> Time.compare (fst b) (fst a))
        >>| (fun l -> List.take l 30)
        >>| List.rev
        >>| List.map ~f:(fun (t, l) -> sprintf "%s\n%s" (Time.to_string t) l)
        >>| String.concat ~sep:"\n\n"
        >>= fun layout_log ->
        Authentication.get_all_sessions ()
        >>| List.mapi ~f:(fun i authhist ->
          div [
            h3 [pcdataf "Session %d" i];
            ul (List.map authhist (fun s ->
              li [codef "[%s]" (Authentication.authentication_state_to_string s)]))
          ])
        >>= fun auth_divs ->
        user_spying ()
        >>= fun spying_divs ->
        return [
          h1 [pcdata "Hitscoreweb Log:"];
          display_output_box (stdout ^ stderr);
          h1 [pcdata "Layout  Log:"];
          display_output_box (layout_log);
          h1 [pcdata "Authenticated Sessions"];
          div auth_divs;
          h1 [pcdata "User Spying"];
          div spying_divs;
        ]
      | false ->
        let configuration = state.Hitscoreweb_state.configuration in
        Authentication.spy_userf "Authentication error in /log" >>= fun () ->
        Template.make_authentication_error ~configuration ~main_title:"Logs"
          (return [pcdataf "You may not view the Logs."])
      end
    in
    Template.default ~title:"Log" content)
