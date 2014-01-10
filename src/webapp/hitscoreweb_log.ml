(* The /log service implementation. *)

open Hitscoreweb_std_server
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
  make_div_scrolled_to_bottom box;
  box

let user_spying () =
  let open Html5 in
  let user_string id =
    Web_data_access.get_person_by_id id
    >>= begin function
    | Some p ->
      return (sprintf "%d (%s, %s)" p#g_id p#family_name p#given_name)
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


let feature_to_string = function
| `fd_passing      ->  "fd_passing"
| `fdatasync       ->  "fdatasync"
| `get_affinity    ->  "get_affinity"
| `get_cpu         ->  "get_cpu"
| `get_credentials ->  "get_credentials"
| `libev           ->  "libev"
| `madvise         ->  "madvise"
| `mincore         ->  "mincore"
| `recv_msg        ->  "recv_msg"
| `send_msg        ->  "send_msg"
| `set_affinity    ->  "set_affinity"
| `wait4           ->  "wait4"

let features = [
  `fd_passing     ;
  `fdatasync      ;
  `get_affinity   ;
  `get_cpu        ;
  `get_credentials;
  `libev          ;
  `madvise        ;
  `mincore        ;
  `recv_msg       ;
  `send_msg       ;
  `set_affinity   ;
  `wait4          ;
]



let lwt_monitor_info =
  let open Lwt in
  let started = ref false in
  let previous = ref (None : float option) in
  let reads = ref [] in
  fun () ->
    begin match !started with
    | false ->
      Lwt.ignore_result begin
        let rec loop () =
          let time = Time.(now () |> to_float) in
          begin match !previous with
          | None -> previous := Some time
          | Some p ->
            (* if time -. p > !maximum then maximum := time -. p else (); *)
            reads := (time, time -. p) :: !reads;
            if List.length !reads > 10 then (
              reads := List.sort 
                  ~cmp:(fun a b -> Float.compare (snd b) (snd a)) !reads 
                       |> (fun l -> List.take l 10);
            );
            previous := Some time;
          end;
          Lwt_main.yield () >>= fun () ->
          loop ()
        in
        loop ()
      end;
      started := true;
    | true -> ();
    end;
    let open Html5 in
    div [
      pcdataf "Maximal unavailability:";
      ul (List.map !reads ~f:(fun (date, delay) ->
          li [pcdataf "%s (%.3f): %f" 
                Time.(of_float date |> to_string) date delay]));
      pcdataf "Features:";
      ul (List.map features ~f:(fun f ->
          li [pcdataf "%s: %b" (feature_to_string f) (Lwt_sys.have f)]));
    ]

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
          h1 [pcdata "Lwt Info"];
          lwt_monitor_info ();
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
