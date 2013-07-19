open Hitscoreweb_std_server

type broker_error =
[ `broker_not_initialized
| `io_exn of exn
| `db_backend_error of Backend.error
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

type classy_error =
[ `Layout of
    Hitscore_layout.Layout.error_location *
      Hitscore_layout.Layout.error_cause
| `db_backend_error of  Hitscore_db_backend.Backend.error
| `io_exn of exn
| `root_directory_not_configured
]
type classy_cache = <
  persons: classy_error Classy.person_element list;
  pgm_runs : classy_error Classy.pgm_run_element list;
  pgm_pools : classy_error Classy.pgm_pool_element list;
  invoicings : classy_error Classy.invoicing_element list;
  pgm_input_libs : classy_error Classy.pgm_input_library_element list;
  pgm_stock_libs:
    (classy_error Classy.pgm_input_library_element
     * classy_error Classy.stock_library_element) list;
>

let classy_cache =
  let r =
    ref (None: (unit ->
                (classy_cache, [ `io_exn of exn ]) t) option) in
  begin fun () ->
    begin match !r with
    | None ->
      let classy_info =
        Data_access.init_retrieval_loop
          ~loop_waiting_time:!_loop_withing_time
          ~log ~allowed_age:!_allowed_age ~maximal_age:!_allowed_age
          ~log_prefix:"classy_pgm_data"
          ~configuration:!_configuration
          ~f:(fun ~configuration ~layout_cache ->
              layout_cache#person >>= fun persons ->
              layout_cache#pgm_run >>= fun pgm_runs ->
              layout_cache#pgm_pool >>= fun pgm_pools ->
              layout_cache#invoicing >>= fun invoicings ->
              layout_cache#pgm_input_library >>= fun pgm_input_libs ->
              while_sequential pgm_input_libs (fun pil ->
                  pil#library#get
                  >>= fun sl ->
                  return (pil, sl))
              >>= fun pgm_stock_libs ->
              return (object
                method persons = persons
                method pgm_runs =  pgm_runs
                method pgm_pools =  pgm_pools
                method invoicings =  invoicings
                method pgm_input_libs =  pgm_input_libs
                method pgm_stock_libs = pgm_stock_libs
              end))
      in
      eprintf "Creation of classy pgm data\n%!";
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
  (* classy_persons () *)
  (* >>= fun _ -> *)
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
  classy_cache ()
  >>= fun classy_cache ->
  return (
    List.find_map classy_cache#persons (fun p ->
        if p#email = id || p#login = Some id ||
           Array.exists p#secondary_emails ((=) id)
        then Some p
        else None))

let find_person id =
  find_person_opt id
  >>= fun op ->
  begin match op with
  | Some s -> return s
  | None -> error (`person_not_found id)
  end

let get_person_by_id id =
  classy_cache ()
  >>= fun classy_cache ->
  return (List.find classy_cache#persons (fun p -> p#g_id = id))

let get_person_by_id_or_error id =
  classy_cache ()
  >>= fun classy_cache ->
  return (List.find classy_cache#persons (fun p -> p#g_id = id))
  >>= begin function
  | Some s -> return s
  | None -> error (`person_not_found (Int.to_string id))
  end

let person_by_pointer p =
  let open Layout.Record_person in
  get_person_by_id p.id
  >>= begin function
  | Some s -> return s
  | None -> error (`person_not_found (Int.to_string p.id))
  end

let wrap_action f x =
  f x
  >>< begin function
  | Ok o -> return o
  | Error e -> error (`classy_data_access e)
  end
