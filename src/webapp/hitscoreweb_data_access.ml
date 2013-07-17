open Hitscoreweb_std

type broker_error =
[ `broker_not_initialized
| `io_exn of exn
| `db_backend_error of Hitscoreweb_std.Backend.error
| `Layout of
        Hitscore_layout.Layout.error_location *
          Hitscore_layout.Layout.error_cause
| `pg_exn of exn ]

let _global_broker : broker_error Broker.t option ref = ref None

let _global_timeout = ref 500.

let broker () =
  match !_global_broker with
  | Some b -> return b
  | None -> error `broker_not_initialized

let rec update  ~configuration () =
  with_database configuration (fun ~dbh ->
    begin match !_global_broker with
    | Some b ->
      Broker.reload b ~dbh ~configuration
      >>= fun () ->
      logf "Broker reloaded"
    | None -> return ()
    end)
  >>= fun () ->
  wrap_io Lwt_unix.sleep !_global_timeout
  >>= fun () ->
  update ~configuration ()



let _loop_withing_time = ref 5.
let _allowed_age = ref 60.
let _maximal_age = ref 900.
let _configuration = ref (Configuration.configure ())

type classy_persons_error =
[ `Layout of
    Hitscore_layout.Layout.error_location *
      Hitscore_layout.Layout.error_cause
| `db_backend_error of  Hitscore_db_backend.Backend.error
| `io_exn of exn
| `root_directory_not_configured ]

let classy_persons =
  let r =
    ref (None: (unit ->
                (classy_persons_error Hitscore_data_access_types.classy_persons_information,
                 [ `io_exn of exn ]) t) option) in
  begin fun () ->
    begin match !r with
    | None ->
      let classy_info =
        Data_access.init_classy_persons_information_loop
          ~loop_waiting_time:!_loop_withing_time
          ~log ~allowed_age:!_allowed_age ~maximal_age:!_allowed_age
          ~configuration:!_configuration
      in
      eprintf "Creation of classy persons\n%!";
      r := Some classy_info;
      classy_info ()
      >>= fun c ->
      return c
    | Some f -> f () >>= return
    end
    >>< begin function
    | Ok o -> return o
    | Error (`io_exn e) -> error (`io_exn e)
    end
  end

let init ~loop_time ~configuration () =
  _global_timeout := loop_time;
  _configuration := configuration;
  classy_persons ()
  >>= fun _ ->
  with_database configuration (fun ~dbh ->
    let broker_mutex = Lwt_mutex.create () in
    let mutex =
      ((fun () -> wrap_io Lwt_mutex.lock broker_mutex),
       (fun () -> Lwt_mutex.unlock broker_mutex)) in
    Broker.create ~mutex ~dbh ~configuration ()
    >>= fun broker ->
    logf "Broker created" >>= fun () ->
    _global_broker := Some broker;
    return ())
  >>= fun () ->
  wrap_io Lwt_unix.sleep !_global_timeout
  >>= fun () ->
  update ~configuration ()


let find_person_opt id =
  classy_persons ()
  >>= fun classy_persons_info ->
  return (
    List.find_map classy_persons_info#persons (fun p ->
      if p#t#email = id || p#t#login = Some id ||
        Array.exists p#t#secondary_emails ((=) id)
      then Some p#t#g_t
      else None))

let find_person id =
  find_person_opt id
  >>= fun op ->
  begin match op with
  | Some s -> return s
  | None -> error (`person_not_found id)
  end

let get_person_by_id id =
  classy_persons ()
  >>= fun classy_persons_info ->
  return (List.find classy_persons_info#persons (fun p -> p#t#g_id = id))


let person_by_pointer p =
  let open Layout.Record_person in
  broker ()
  >>= fun broker ->
  let c = Broker.current_dump broker in
  return (List.find_exn c.Layout.person ~f:(fun x -> x.g_id = p.id))

let modify_person ~dbh ~person =
  bind_on_error (broker ()
                 >>= fun broker ->
                 Broker.modify_person broker ~dbh ~person)
    (fun e -> error (`broker_error e))