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

module File_cache = struct

  module String_map = Core.Std.Map.Poly

  let _run_param_cache =
    ref (String_map.empty:
           (string,
            Hitscore_interfaces.Hiseq_raw_information.clusters_info option array)
           String_map.t)
      
  let get_clusters_info (path:string) =
    let make file = 
      read_file file >>= fun xml_s ->
      let xml =
        Xml_tree.(in_tree (make_input (`String (0, xml_s)))) in
      Hiseq_raw.clusters_summary (snd xml) |! of_result
    in
    match String_map.find !_run_param_cache path with
    | Some r -> return r
    | None ->
      make path >>= fun res ->
      _run_param_cache := String_map.add ~key:path ~data:res !_run_param_cache;
      return res
        
  let _demux_summary_cache =
    ref (String_map.empty:
           (string,
            Hitscore_interfaces.B2F_unaligned_information.demux_summary) String_map.t)
      
  let get_demux_summary path =
    let make file = 
      read_file file >>= fun xml_s ->
      let xml =
        Xml_tree.(in_tree (make_input (`String (0, xml_s)))) in
      B2F_unaligned.flowcell_demux_summary (snd xml)
      |! of_result
    in
    match String_map.find !_demux_summary_cache path with
    | Some r -> return r
    | None ->
      make path >>= fun res ->
      _demux_summary_cache :=
        String_map.add ~key:path ~data:res !_demux_summary_cache;
      return res

  type bp_fastx_quality_stats = {
    bfxqs_column: float;
    bfxqs_count: float;
    bfxqs_min: float;
    bfxqs_max: float;
    bfxqs_sum: float;
    bfxqs_mean: float;
    bfxqs_Q1: float;
    bfxqs_med: float;
    bfxqs_Q3: float;
    bfxqs_IQR: float;
    bfxqs_lW: float;
    bfxqs_rW: float;
    bfxqs_A_Count: float;
    bfxqs_C_Count: float;
    bfxqs_G_Count: float;
    bfxqs_T_Count: float;
    bfxqs_N_Count: float;
    bfxqs_Max_count: float;
  }
  type fastx_quality_stats = bp_fastx_quality_stats list
      
  let _fastx_quality_stats_cache =
    ref (String_map.empty: (string, fastx_quality_stats) String_map.t)
      
  let get_fastx_quality_stats path =
    match String_map.find !_fastx_quality_stats_cache path with
    | Some r -> return r
    | None ->
      read_file path >>| String.split ~on:'\n'
      >>| List.map ~f:(fun l -> String.split ~on:'\t' l)
      >>| List.filter ~f:(function [] | [_] -> false | _ -> true)
      >>= (function 
      | [] | _ :: [] -> error (`empty_fastx_quality_stats path)
      | h :: t ->
        try List.map t ~f:(List.map ~f:Float.of_string) |! return with
          e -> error (`error_in_fastx_quality_stats_parsing (path, t)))
      >>| List.map ~f:(function
      | [bfxqs_column; bfxqs_count; bfxqs_min; bfxqs_max;
         bfxqs_sum; bfxqs_mean; bfxqs_Q1; bfxqs_med;
         bfxqs_Q3; bfxqs_IQR; bfxqs_lW; bfxqs_rW;
         bfxqs_A_Count; bfxqs_C_Count; bfxqs_G_Count; bfxqs_T_Count; bfxqs_N_Count;
         bfxqs_Max_count;] ->
        Some {
          bfxqs_column; bfxqs_count; bfxqs_min; bfxqs_max;
          bfxqs_sum; bfxqs_mean; bfxqs_Q1; bfxqs_med;
          bfxqs_Q3; bfxqs_IQR; bfxqs_lW; bfxqs_rW;
          bfxqs_A_Count; bfxqs_C_Count; bfxqs_G_Count; bfxqs_T_Count; bfxqs_N_Count;
          bfxqs_Max_count;}
      | _ -> None)
      >>| List.filter_opt
      >>= function
      | [] -> error (`error_in_fastx_quality_stats_parsing (path, []))
      | l ->
        _fastx_quality_stats_cache :=
          String_map.add ~key:path ~data:l !_fastx_quality_stats_cache;
        return l
          
          
end
