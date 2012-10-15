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
  
let init ~loop_time ~configuration () =
  _global_timeout := loop_time;
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
  broker ()
  >>= fun broker ->
  Broker.find_person broker id
    
let find_person id =
  find_person_opt id
  >>= fun op ->
  begin match op with
  | Some s -> return s
  | None -> error (`person_not_found id)
  end

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

