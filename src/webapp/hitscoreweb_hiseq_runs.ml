
open Hitscoreweb_std_server

(* Testing flow-stuff: *)
(* let (>>!) m f = bind_on_error m ~f in *)
let flow_some opt ~err =
  match opt with
  | Some s -> return s
  | None -> error err

module Web_data_access = Hitscoreweb_data_access

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

module Persons_service = Hitscoreweb_persons

let flowcell_cell ~dbh fc_p =
  let open Html5 in
  Layout.Record_flowcell.(
    Access.Flowcell.get ~dbh fc_p >>= fun fc ->
    let lane_nb = ref 0 in
    while_sequential (Array.to_list fc.g_value.lanes) ~f:(fun lane_p ->
      Layout.Record_lane.(
        Access.Lane.get ~dbh lane_p >>= fun lane ->
        while_sequential (Array.to_list lane.g_value.contacts)
          ~f:(Persons_service.person_link ~style:`family_name dbh)
        >>= fun links ->
        incr lane_nb;
        return (Array.to_list lane.g_value.contacts,
                (!lane_nb, links,
                 lane.g_value.requested_read_length_1,
                 lane.g_value.requested_read_length_2))))
    >>= fun lanes_info ->
    let keys =
      List.filter lanes_info ~f:(fun x -> fst x <> [])
      |> List.map ~f:fst |> List.dedup in
    let lanes_of l =
      List.map l ~f:(fun (lane, _, _, _) -> sprintf "%d" lane)
      |> String.concat ~sep:", " in
    let links_of l =
      try let (_, links,_ ,_) = List.hd_exn l in
          interleave_list ~sep:(pcdata ", ") links
      with e -> [] in
    let info_on_lanes =
      interleave_list ~sep:[ pcdata "; "; br () ]
        (List.map keys ~f:(fun k ->
          let vals = List.filter lanes_info ~f:(fun a -> fst a = k) in
          let plural = if List.length vals = 1 then "" else "s" in
          let lanes = List.map vals snd |> lanes_of in
            (* Put lane numbers first to allow sorting and then removing them *)
          (lanes, pcdataf "Lane%s %s: " plural lanes
            :: (List.map vals snd |> links_of)))
          |> List.sort ~cmp:compare |> List.map ~f:snd) in
    let run_type =
      try
        let (_, (_, _, r1 ,r2o)) =
          List.find_exn lanes_info ~f:(fun a -> fst a = List.hd_exn keys) in
        match r2o with
        | None -> sprintf "SE %d" r1
        | Some r2 -> sprintf "PE %dx%d" r1 r2
      with e -> "NO RUN-TYPE" in
    let link =
      Template.a_link Services.flowcell [strong [pcdataf "%s" fc.g_value.serial_name]]
        fc.g_value.serial_name in
    return (`sortable (fc.g_value.serial_name,
                       [link; pcdataf " — %s" run_type; br ()]
                       @ (List.concat info_on_lanes))))

let hiseq_runs ~configuration =
  let open Html5 in
  let open Template in
  with_database ~configuration (fun ~dbh ->
    let layout = Classy.make dbh in
    layout#hiseq_run#all >>= fun hiseq_runs ->
    while_sequential hiseq_runs (fun hsr ->
      let sfc p =
        Option.value_map p ~default:(return (`text [pcdataf "—"]))
          ~f:(fun p -> flowcell_cell ~dbh p#pointer) in
      let date_cell =
        `sortable (Time.to_string hsr#date,
          [strong [
             pcdata hsr#sequencer; pcdata ": "; br ();
             pcdata (Time.to_local_date hsr#date |> Date.to_string)]]) in
      sfc hsr#flowcell_a >>= fun fca ->
      sfc hsr#flowcell_b >>= fun fcb ->
      return ([date_cell ; fca; fcb])))
  >>= fun rows ->
  let sorted =
    List.sort ~cmp:(fun l1 l2 ->
      compare (List.hd_exn l1) (List.hd_exn l2) * -1) rows in
  return (content_table
            ([ `head [pcdata "Run"];
               `head [pcdata "Flowcell A"];
               `head [pcdata "Flowcell B"]; ]
             :: sorted))

let person_flowcells ~configuration (person : Layout.Record_person.pointer) =
  let open Html5 in
  let open Template in

  let start_time = Time.(now () |> to_float) in

  Web_data_access.classy_cache ()
  >>= fun classy_cache ->
  flow_some  ~err:(`hiseq_runs (`cannot_retrieve_person_affairs person))
    (List.find classy_cache#classy_persons#persons
       (fun p -> p#t#g_pointer = person))
  >>= fun person_affairs ->
  let filtered_meta_hiseq_runs =
    List.filter_map classy_cache#meta_hiseq_runs (fun mhr ->
        let filtered_deliveries =
          List.filter_map mhr#deliveries (fun deliv ->
              let filtered_lanes =
                List.filter deliv#lanes (fun l ->
                    let contacts = Array.to_list l#lane#contacts in
                    List.exists contacts (fun c -> c#pointer = person))
              in
              match filtered_lanes with
              | [] ->
                dbg "%s filtered OUT" deliv#delivery_dir#directory;
                None
              | l ->
                Some (object
                  method delivery = deliv#delivery
                  method delivery_dir = deliv#delivery_dir
                  method demux = deliv#demux
                  method lanes = l
                end))
        in
        match filtered_deliveries with
        | [] -> None
        | l ->
          Some (object
            method hr = mhr#hr
            method a_or_b = mhr#a_or_b
            method flowcell = mhr#flowcell
            method deliveries = l
          end))
  in

  let display_run hiseq_meta_run =
    let hr = hiseq_meta_run#hr in
    let a_or_b = hiseq_meta_run#a_or_b in
    let flowcell = hiseq_meta_run#flowcell in
    let deliveries = hiseq_meta_run#deliveries in
    let run_title =
      pcdataf "%s Run (Flowcell %s: %s)"
        (hr#date |> Date.of_time |> Date.to_string)
        a_or_b (flowcell#serial_name) in
    let delivery_table delivery =
      let base_head =
        [ `head_cell Msg.lane;
          `head_cell Msg.library_name;
          `head_cell Msg.library_project;
          `head_cell Msg.library_description ] in
      let demux_sum_opt =
        Option.(
          delivery#demux#unaligned
          >>= fun u ->
          u#dmux_summary) in
      let summary_head =
        match demux_sum_opt with
        | None -> []
        | Some _ ->
          [ `head_cell Msg.number_of_reads;
            `head_cell Msg.percent_bases_over_q30;
            `head_cell Msg.mean_qs ] in
      let sorted_lanes =
        List.sort delivery#lanes
          ~cmp:(fun l1 l2 -> Int.compare l1#lane_index l2#lane_index) in
      content_table (
        (base_head @ summary_head)
        :: List.map sorted_lanes (fun l ->
            let index_text = l#lane_index |> Int.to_string in
            let libraries_subtable =
              List.sort ~cmp:(fun l1 l2 ->
                  String.compare
                    l1#classy_library#stock#name l1#classy_library#stock#name)
                l#classy_libraries
              |> List.filter ~f:(fun cl ->
                    cl#classy_library#stock#name <> "PhiX_v3"
                    && cl#classy_library#stock#name <> "PhiX_v2")
              |> List.map ~f:(fun cl ->
                  let libname = pcdata cl#classy_library#stock#name in
                  let libproj =
                    Option.value_map ~f:pcdata ~default:(pcdata "")
                      cl#classy_library#stock#project in
                  let libdesc =
                    Option.value_map ~f:pcdata ~default:(pcdata "")
                      cl#classy_library#stock#description in
                  let stats =
                    let nb0 f = `number (sprintf "%.0f", f) in
                    let nb2 f = `number (sprintf "%.2f", f) in
                    let module Bui = Hitscore_interfaces.B2F_unaligned_information in
                    let open Option in
                    demux_sum_opt
                    >>= fun dmx ->
                    List.find dmx.(l#lane_index - 1)
                      (fun x -> x.Bui.name = cl#classy_library#stock#name)
                    >>= fun found ->
                    return [nb0 found.Bui.cluster_count;
                            nb2 (100. *. found.Bui.yield_q30 /. found.Bui.yield);
                            nb2 (found.Bui.quality_score_sum /. found.Bui.yield)]
                  in
                  [`text [libname]; `text [libproj]; `text [libdesc];]
                  @ Option.value ~default:[] stats)
            in
            [
              `sortable (index_text, [pcdata index_text]);
              `subtable libraries_subtable;
            ])
      )
    in
    content_section run_title
      (content_list
         (List.map deliveries ~f:(fun delivery ->
              let title =
                span [
                  codef "%s" delivery#delivery_dir#directory;
                  pcdata " on ";
                  html_of_cluster delivery#delivery_dir#host;
                ]  in
              let section_content = delivery_table delivery in
              content_section title  section_content)))
  in

  let middle_time = Time.(now () |> to_float) in
  let sections = List.map ~f:display_run filtered_meta_hiseq_runs in
  let end_time = Time.(now () |> to_float) in
  let content =
    content_list ((content_paragraph [pcdataf "DEBUG INFO: %f s, rendering: %f s"
                                        (end_time -. start_time)
                                        (end_time -. middle_time)
                                     ])
                  :: sections)
  in
  return content



let make configuration =
  (fun () () ->
    Template.default ~title:"HiSeq Runs"
      (Authentication.authorizes (`view `all_hiseq_runs)
       >>= fun can_view_hiseq_runs ->
       Authentication.authorizes (`view `user_hiseq_runs)
       >>= fun can_view_all_flowcells ->
       if can_view_hiseq_runs
       then

         person_flowcells ~configuration (Layout.Record_person.unsafe_cast 3150)
         >>= fun content ->
           Template.make_content ~configuration
             ~main_title:"HiSeq Runs" (return content)

         (* Template.make_content ~configuration *)
           (* ~main_title:"HiSeq Runs" (hiseq_runs configuration) *)
       else if can_view_all_flowcells
       then
         begin
           Authentication.user_logged () >>= fun user_opt ->
           flow_some user_opt (`hiseq_runs (`no_logged_user))
           >>= fun {Authentication.person; _} ->
           Template.make_content ~configuration
             ~main_title:"HiSeq Runs"
             (person_flowcells ~configuration person)
         end
       else
         Template.make_authentication_error ~configuration
           ~main_title:"HiSeq Runs"
           (return [Html5.pcdataf "You may not view anything here."])))
