(*D

`Web_data_access`: Manage the Cache of the Data-base
----------------------------------------------------


*)
open Hitscoreweb_std_server

(*D

### The Classy Cache

These global variables are used by the asynchronous update of the
“classy cache”.

The configuration must be set before the first call to `classy_cache`
(i.e. use the function `init`).

*)
let _loop_withing_time = ref 5.
let _allowed_age = ref 60.
let _maximal_age = ref 900.
let _configuration = ref (Configuration.configure ())

(*D

The use of the `Layout.Classy` module forces us to fix this error
type:
*)
type classy_error =
[ `Layout of
    Hitscore_layout.Layout.error_location *
      Hitscore_layout.Layout.error_cause
| `db_backend_error of  Hitscore_db_backend.Backend.error
| `io_exn of exn
| `root_directory_not_configured
]

(*D

This is a description of the contents of the “classy cache”:

*)
type classy_cache = <
  persons: classy_error Classy.person_element list;
  pgm_runs : classy_error Classy.pgm_run_element list;
  pgm_pools : classy_error Classy.pgm_pool_element list;
  invoicings : classy_error Classy.invoicing_element list;
  pgm_input_libs : classy_error Classy.pgm_input_library_element list;
  pgm_stock_libs:
    (classy_error Classy.pgm_input_library_element
     * classy_error Classy.stock_library_element) list;
  hiseq_runs       : classy_error Classy.hiseq_run_element list;
  hiseq_flowcells  : classy_error Classy.flowcell_element list;
  hiseq_lanes      : classy_error Classy.lane_element list;
  hiseq_input_libs : classy_error Classy.input_library_element list;
  stock_libs : classy_error Classy.stock_library_element list;
  classy_persons: classy_error Data_access_types.classy_persons_information;
  classy_libraries: classy_error Data_access_types.classy_libraries_information;

  meta_hiseq_runs: <
    a_or_b: string;
    flowcell : classy_error Classy.flowcell_element;
    hr : classy_error Classy.hiseq_run_element;
    deliveries: <
      delivery: classy_error Data_access_types.delivery;
      delivery_dir: classy_error Classy.client_fastqs_dir_element;
      demux: classy_error Data_access_types.demultiplexing;
      lanes: <
        classy_libraries: <
          classy_library: classy_error Data_access_types.classy_library;
          delivered_demuxes: classy_error Data_access_types.demultiplexing list;
          input_library: classy_error Classy.input_library_element;
          submission: classy_error Data_access_types.submission
        > list;
        flowcell : classy_error Classy.flowcell_element;
        lane: classy_error Classy.lane_element;
        lane_index: int;
        people : classy_error Data_access_types.person list;
      > list;
    > list;
  > list;
>

(*D

This function is the part of `classy_cache` that creates/updates the
`meta_hiseq_runs` field. It basically reorganizes the data so that the
`/hiseq_runs` service for normal users just has to filter it for the
given user and display.

Note that when this code was part of the `/hiseq_runs` service the
page loading time was much much bigger.
*)
let meta_hiseq_runs
    ~hiseq_runs ~hiseq_flowcells ~classy_persons
    ~hiseq_input_libs ~classy_libraries
    ~hiseq_lanes ~invoicings =
  List.map hiseq_runs (fun hr ->
      let find_flowcell fcopt =
        Option.bind fcopt (fun fc ->
            List.find hiseq_flowcells (fun f -> f#g_id = fc#id))
      in
      let flowcell_a = find_flowcell hr#flowcell_a in
      let flowcell_b = find_flowcell hr#flowcell_b in
      let lane_info idx lane fc =
        let contacts = Array.to_list lane#contacts in
        let people =
          List.filter_map contacts (fun ct ->
              List.find classy_persons#persons
                (fun p -> p#t#g_id = ct#id)) in
        let libraries =
          List.filter_map (Array.to_list lane#libraries) (fun lpointer ->
              List.find hiseq_input_libs (fun hil ->
                  hil#g_pointer = lpointer#pointer))
          |> List.filter_map ~f:(fun il ->
              List.find_map classy_libraries#libraries
                (fun cl ->
                   if (cl#stock#g_id = il#library#id)
                   then
                     (match
                       List.find cl#submissions (fun sub ->
                           List.exists sub#lane#inputs
                             (fun i -> i#g_id = il#g_id))
                     with
                     | Some right_submission ->
                       let delivered_demuxes =
                         List.map right_submission#flowcell#hiseq_raws
                           (fun hsr ->
                              List.filter hsr#demultiplexings
                                (fun demux -> demux#deliveries <> []))
                         |> List.concat
                       in
                       Some (object
                         method classy_library = cl
                         method input_library = il
                         method submission = right_submission
                         method delivered_demuxes = delivered_demuxes
                       end)
                     | None -> None)
                   else None))
        in
        object
          method flowcell = fc
          method lane = lane
          method lane_index = idx
          method people = people
          method classy_libraries = libraries
        end
      in
      let lanes_of_flowcell fc =
        List.filter_map hiseq_lanes (fun l ->
            let rec find_map_with_index idx = function
            | [] -> None
            | x :: t ->
              if x#id = l#g_id
              then Some (lane_info idx l fc)
              else find_map_with_index (idx + 1) t
            in
            find_map_with_index 1 (Array.to_list fc#lanes))
      in
      let deliveries_of_flowcell fc =
        let meta_lanes = lanes_of_flowcell fc in
        List.map meta_lanes (fun lane ->
            List.map lane#classy_libraries (fun cl ->
                let delivered_demuxes = cl#delivered_demuxes in
                List.map delivered_demuxes (fun demux ->
                    List.filter_map demux#deliveries (fun d ->
                        Option.bind d#client_fastqs_dir (fun s ->
                            let invoice =
                              List.find invoicings
                                (fun i -> i#g_id = d#oo#invoice#id) in
                            Option.bind invoice (fun invoice ->
                                Option.some_if
                                  (Array.exists lane#lane#contacts
                                     (fun c -> c#id = invoice#pi#id))
                                  (object
                                    method delivery = d
                                    method delivery_dir = s
                                    method demux = demux
                                    method lanes =
                                      List.filter meta_lanes
                                        (fun lane ->
                                           (Array.exists lane#lane#contacts
                                              (fun c -> c#id = invoice#pi#id)))
                                  end)))))))
        |> List.concat
        |> List.concat
        |> List.concat
        |> List.dedup ~compare:(fun a b ->
            Int.compare a#delivery_dir#g_id b#delivery_dir#g_id)
      in

      let make_run a_or_b flowcell deliveries =
        Option.some_if (deliveries <> [])
            (object
              method hr = hr
              method a_or_b = a_or_b
              method flowcell = flowcell
              method deliveries = deliveries
            end) in
        List.filter_opt [
          Option.bind flowcell_a (fun f ->
              make_run "A" f (deliveries_of_flowcell f));
          Option.bind flowcell_b (fun f ->
              make_run "B" f (deliveries_of_flowcell f));
        ])
    |> List.concat

(*D

This is the main function of the module that provides the whole
currently available cache object.

The first time it is called it starts the asynchronous update loop,
see `Hitscore.Data_access.init_retrieval_loop`.

*)
let classy_cache =
  let r =
    ref (None: (unit ->
                (classy_cache, [ `io_exn of exn ]) t) option) in
  begin fun () ->
    begin match !r with
    | None ->
      let errf fmt =
        ksprintf (fun s -> error (`io_exn (Failure ("classy_cache: " ^ s)))) fmt
      in
      let classy_info =
        Data_access.init_retrieval_loop
          ~loop_waiting_time:!_loop_withing_time
          ~log ~allowed_age:!_allowed_age ~maximal_age:!_allowed_age
          ~log_prefix:"hsw_classy_cache"
          ~configuration:!_configuration
          ~f:(fun ~configuration ~layout_cache ->
              Data_access.make_classy_persons_information
                ~configuration ~layout_cache
              >>= fun classy_persons ->
              Data_access.make_classy_libraries_information
                ~configuration ~layout_cache
              >>= fun classy_libraries ->
              layout_cache#person >>= fun persons ->
              layout_cache#pgm_run >>= fun pgm_runs ->
              layout_cache#pgm_pool >>= fun pgm_pools ->
              layout_cache#invoicing >>= fun invoicings ->
              layout_cache#pgm_input_library >>= fun pgm_input_libs ->
              layout_cache#stock_library >>= fun stock_libs ->
              layout_cache#hiseq_run         >>= fun hiseq_runs ->
              layout_cache#flowcell    >>= fun hiseq_flowcells ->
              layout_cache#lane        >>= fun hiseq_lanes->
              layout_cache#input_library   >>= fun hiseq_input_libs->
              while_sequential pgm_input_libs (fun pil ->
                  match
                    List.find stock_libs (fun sl -> sl#g_id = pil#library#id)
                  with
                  | Some s -> return (pil, s)
                  | None -> errf "pgm input library %d has no stock (%d?)"
                              pil#g_id pil#library#id)
              >>= fun pgm_stock_libs ->
              let meta_hiseq_runs =
                meta_hiseq_runs
                  ~hiseq_runs ~hiseq_flowcells ~classy_persons
                  ~hiseq_input_libs ~classy_libraries
                  ~hiseq_lanes ~invoicings in
              return (object
                method persons = persons
                method pgm_runs =  pgm_runs
                method pgm_pools =  pgm_pools
                method invoicings =  invoicings
                method pgm_input_libs =  pgm_input_libs
                method pgm_stock_libs = pgm_stock_libs
                method stock_libs = stock_libs
                method classy_persons = classy_persons
                method classy_libraries = classy_libraries
                method hiseq_runs       = hiseq_runs
                method hiseq_flowcells  = hiseq_flowcells
                method hiseq_lanes      = hiseq_lanes
                method hiseq_input_libs = hiseq_input_libs
                method meta_hiseq_runs = meta_hiseq_runs
              end))
      in
      eprintf "Creation of classy data\n%!";
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

(*D

The purpose of the `init` function is to set the `_configuration`
global variable and to start the update loop.

*)
let init ~configuration () =
  _configuration := configuration;
  classy_cache ()
  >>= fun _ ->
  return ()

(*D

### Finding People

These functions are “shortcuts” which use the `classy_cache`
themselves, to find users in different forms.

This one finds a “classy person” option using it's emails addresses or
login.
*)
let find_person_opt id =
  classy_cache ()
  >>= fun classy_cache ->
  return (
    List.find_map classy_cache#persons (fun p ->
        if p#email = id || p#login = Some id ||
           Array.exists p#secondary_emails ((=) id)
        then Some p
        else None))

(*D

This one is like the previous but fails on `None`.

*)
let find_person id =
  find_person_opt id
  >>= fun op ->
  begin match op with
  | Some s -> return s
  | None -> error (`person_not_found id)
  end

(*D

These three functions find a “classy person” by data-base identifier
(`int`) or typed pointer (`Layout.Record_person.pointer`).

*)
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

(*D

### Additional Functions

This function helps the error management with the `Flow` monad by
encapsulating errors into <code>`classy_data_access of 'error</code>.

*)
let wrap_action f x =
  f x
  >>< begin function
  | Ok o -> return o
  | Error e -> error (`classy_data_access e)
  end
