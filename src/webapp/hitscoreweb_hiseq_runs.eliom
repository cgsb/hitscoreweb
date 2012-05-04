
open Hitscoreweb_std

module Data_access = Hitscoreweb_data_access

module Queries = Hitscoreweb_queries

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

module Persons_service = Hitscoreweb_persons
    
let flowcell_cell ~dbh fc_p =
  let open Html5 in
  Layout.Record_flowcell.(
    get ~dbh fc_p >>= fun fc ->
    let lane_nb = ref 0 in
    of_list_sequential (Array.to_list fc.lanes) ~f:(fun lane_p ->
      Layout.Record_lane.(
        get ~dbh lane_p >>= fun lane ->
        of_list_sequential (Array.to_list lane.contacts)
          ~f:(Persons_service.person_link ~style:`family_name dbh)
        >>= fun links ->
        incr lane_nb;
        return (Array.to_list lane.contacts,
                (!lane_nb, links,
                 lane.requested_read_length_1, lane.requested_read_length_2))))
    >>= fun lanes_info -> 
    let keys =
      List.filter lanes_info ~f:(fun x -> fst x <> []) 
      |! List.map ~f:fst |! List.dedup in
    let lanes_of l =
      List.map l ~f:(fun (lane, _, _, _) -> sprintf "%d" lane)
      |! String.concat ~sep:", " in
    let links_of l =
      let (_, links,_ ,_) = List.hd_exn l in
      interleave_list ~sep:(pcdata ", ") links in
    let info_on_lanes =
      interleave_list ~sep:[ br () ]
        (List.map keys ~f:(fun k ->
          let vals = List.filter lanes_info ~f:(fun a -> fst a = k) in
          let plural = if List.length vals = 1 then "" else "s" in
          let lanes = List.map vals snd |! lanes_of in
            (* Put lane numbers first to allow sorting and then removing them *)
          (lanes, pcdataf "Lane%s %s: " plural lanes
            :: (List.map vals snd |! links_of)))
          |! List.sort ~cmp:compare |! List.map ~f:snd) in
    let run_type =
      let (_, (_, _, r1 ,r2o)) =
        List.find_exn lanes_info ~f:(fun a -> fst a = List.hd_exn keys) in
      match r2o with
      | None -> sprintf "SE %ld" r1
      | Some r2 -> sprintf "PE %ldx%ld" r1 r2 in
    let link =
      Template.a_link Services.flowcell [strong [pcdataf "%s" fc.serial_name]]
        fc.serial_name in
    return (`sortable (fc.serial_name,
                       [link; pcdataf " — %s" run_type; br ()]
                       @ (List.concat info_on_lanes))))
    
let hiseq_runs ~configuration =
  let open Html5 in
  let open Template in
  Hitscore_lwt.with_database ~configuration ~f:(fun ~dbh ->
    Layout.Record_hiseq_run.(
      get_all ~dbh >>= fun hiseq_runs ->
      of_list_sequential hiseq_runs ~f:(fun hsr ->
        get ~dbh hsr >>= fun {date; flowcell_a; flowcell_b} ->
        Layout.Record_flowcell.(
          let sfc =
            Option.value_map ~default:(return (`text [pcdataf "—"]))
              ~f:(flowcell_cell ~dbh) in
          let date_cell =
            `sortable (Time.to_string date,
                       [strong [
                         pcdata (Time.to_local_date date |! Date.to_string)]])
          in
          sfc flowcell_a >>= fun fca ->
          sfc flowcell_b >>= fun fcb ->
          return ([date_cell ; fca; fcb]))))
    >>= fun rows ->
    let sorted =
      List.sort ~cmp:(fun l1 l2 ->
        compare (List.hd_exn l1) (List.hd_exn l2) * -1) rows in
    return (content_table 
              ([ `head [pcdata "Run date"];
                 `head [pcdata "Flowcell A"];
                 `head [pcdata "Flowcell B"]; ]
               :: sorted)))

let lanes_table broker lanes dmux_sum_opt =
  let open Html5 in
  let open Template in
  let module Broker = Hitscore_lwt.Broker in
  let open Option in
  let lib_name lib_in_lan =
    let p = lib_in_lan.Broker.lil_stock.Layout.Record_stock_library.project in
    let n = lib_in_lan.Broker.lil_stock.Layout.Record_stock_library.name in
    sprintf "%s%s" Option.(value_map p ~default:"" ~f:(sprintf "%s.")) n in
  let lib_link l =
    Template.a_link Services.libraries
      [pcdata (lib_name l)] ([`basic], [lib_name l]) in
  let nb0 f = `number (sprintf "%.0f", f) in 
  let nb2 f = `number (sprintf "%.2f", f) in 
  let one_lane l =
    let without_phix =
      List.filter l.Broker.lane_libraries ~f:(fun lib ->
        lib.Broker.lil_stock.Layout.Record_stock_library.name <> "PhiX_v3"
        && lib.Broker.lil_stock.Layout.Record_stock_library.name <> "PhiX_v2")
    in
    `subtable 
      (List.map without_phix (fun lib ->
        let stats =
          let module Bui = Hitscore_interfaces.B2F_unaligned_information in
          dmux_sum_opt
          >>= fun dmx ->
          List.find dmx.(l.Broker.lane_index - 1)
            (fun x ->
              x.Bui.name = lib.Broker.lil_stock.Layout.Record_stock_library.name)
          >>= fun found ->
          return [nb0 found.Bui.cluster_count;
                  nb2 (100. *. found.Bui.yield_q30 /. found.Bui.yield);
                  nb2 (found.Bui.quality_score_sum /. found.Bui.yield)]
        in
        let name = lib_name lib in
        let desc =
          value ~default:""
            lib.Broker.lil_stock.Layout.Record_stock_library.description in
        [ `sortable (name, [lib_link lib]);
          `sortable (desc, [pcdata desc]) ] 
        @ (value ~default:[] stats))) 
  in
  let base_head =
    [ `head_cell Msg.lane; `head_cell Msg.library_qn;
      `head_cell Msg.library_description ] in
  let summary_head =
    match dmux_sum_opt with
    | None -> []
    | Some o ->
      [ `head_cell Msg.number_of_reads;
        `head_cell Msg.percent_bases_over_q30;
        `head_cell Msg.mean_qs ] in
  ((base_head @ summary_head)
   ::
     (List.map lanes (fun l ->
       [ `text [pcdataf "Lane %d" l.Broker.lane_index];
         one_lane l ])))
    
let simple_lanes_table broker lanes =
  lanes_table broker lanes None

let lanes_table_with_stats broker lanes dmux_sum =
  lanes_table broker lanes dmux_sum
    
    
let person_flowcells ~configuration =
  let open Html5 in
  let open Template in
  let module Broker = Hitscore_lwt.Broker in

    (* Testing flow-stuff: *)
  let (>>!) m f = bind_on_error m ~f in
  let flow_some opt ~err =
    match opt with
    | Some s -> return s
    | None -> error err in
  
  Data_access.broker () >>= fun broker ->
  Authentication.user_logged () >>= fun user_opt ->
  flow_some user_opt (`hiseq_runs (`no_logged_user))
  >>= fun {Authentication.person; _} ->
  (Broker.person_affairs broker person.Layout.Record_person.id
   >>! (function
   | `person_not_found person ->
     error (`hiseq_runs (`cannot_retrieve_person_affairs person))))
  >>= fun affairs ->
  of_list_sequential affairs.Broker.pa_flowcells (fun fc ->
    of_list_sequential  fc.Broker.ff_runs (fun run ->
      Broker.delivered_demultiplexings broker run
      >>| List.filter_map ~f:(fun ddmux ->
        let person_deliveries =
          List.filter ddmux.Broker.ddmux_deliveries ~f:(fun (fpud, cfd) ->
            List.exists fc.Broker.ff_lanes
              ~f:(fun lane ->
                List.exists lane.Broker.lane_invoices
                  ~f:(fun invoice ->
                    fpud.Layout.Function_prepare_unaligned_delivery.
                      invoice.Layout.Record_invoicing.id  
                    = invoice.Layout.Record_invoicing.g_id)))
        in
        if person_deliveries = []
        then None
        else Some (ddmux, person_deliveries))
      >>= fun run_deliveries ->
      of_list_sequential run_deliveries ~f:(fun (ddmux, pdeliv) ->
        let dmux_summary_m =
          Data_access.File_cache.get_demux_summary ddmux.Broker.ddmux_summary_path
        in
        double_bind dmux_summary_m
          ~ok:(fun dmux_summary -> return (ddmux, pdeliv, Some dmux_summary))
          ~error:(fun e ->
            eprintf "Error getting: %s\n%!" ddmux.Broker.ddmux_summary_path;
            return (ddmux, pdeliv, None)))
      >>= fun run_deliveries_with_stats ->
      return (run, run_deliveries_with_stats))
    >>= fun runs_and_deliveries ->
    let run_subsections =
      List.map runs_and_deliveries (fun (hrt, deliveries) ->
        let date = hrt.Broker.hr_t.Layout.Record_hiseq_run.date in
        let run_title =
          pcdataf "%s Run" (Time.to_local_date date |! Date.to_string) in
        let deliveries_list =
          let delivery_title cfd =
            span [
              pcdata "Delivery ";
              codef "%s" cfd.Layout.Record_client_fastqs_dir.directory;
            ] in
          List.map deliveries (fun (ddmux, delivp, dmux_sum) ->
            List.map delivp (fun (fpub, cfd) ->
              content_section (delivery_title cfd)
                (lanes_table_with_stats broker fc.Broker.ff_lanes dmux_sum
                 |! content_table))
          ) |! List.concat;
        in
        if deliveries = []
        then 
          content_section run_title
            (simple_lanes_table broker fc.Broker.ff_lanes |! content_table)
        else
          content_section run_title (content_list deliveries_list)
      )
    in
    return (content_section (pcdataf "Flowcell %s" fc.Broker.ff_id)
              (content_list run_subsections)))
  >>= fun sections ->
  let content = content_list sections in
  return content


    
let make configuration =
  (fun () () ->
    Template.default ~title:"HiSeq 2000 Runs"
      (Authentication.authorizes (`view `all_hiseq_runs)
       >>= fun can_view_hiseq_runs ->
       Authentication.authorizes (`view `all_flowcells)
       >>= fun can_view_all_flowcells ->
       if can_view_hiseq_runs
       then
         Template.make_content ~configuration
           ~main_title:"HiSeq 2000 Runs" (hiseq_runs configuration)
       else if can_view_all_flowcells
       then
         Template.make_content ~configuration
           ~main_title:"HiSeq 2000 Runs" (person_flowcells ~configuration)
       else
         Template.make_authentication_error ~configuration
           ~main_title:"HiSeq 2000 Runs" 
           (return [Html5.pcdataf "You may not view anything here."])))


