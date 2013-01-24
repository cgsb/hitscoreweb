
{shared{
open Hitscoreweb_std
}}
module Web_data_access = Hitscoreweb_data_access
module Authentication = Hitscoreweb_authentication
module Template = Hitscoreweb_template
module Services = Hitscoreweb_services
module Persons_service = Hitscoreweb_persons


let sortable_people_cell people =
  let open Html5 in
  let sortability =
    List.map people trd3 |! String.concat ~sep:", " in
  let cell =
    (List.map people (fun (f, l, e) ->
      [
        Template.a_link Services.persons
          [ksprintf Html5.pcdata "%s %s" f l] (Some true, [e]);
        br () ]) |! List.concat)
    @ [
      if List.length people > 1 then
        small [
          pcdata "(";
          Template.a_link Services.persons [pcdata "all"]
            (None, List.map people (fun (f, l, e) -> e));
          pcdata ")"
        ]
      else
        span []
    ]
  in
  (sortability, cell)


let sortable_libraries_cell libs =
  let open Html5 in
  let qname = function (l, None) -> l | (l, Some p) -> sprintf "%s.%s" p l in
  let sortability = String.concat ~sep:", " (List.map ~f:qname libs) in
  let paragraphs =
    List.sort ~cmp:(fun (_, a) (_, b) -> compare a b) libs
    |! List.group ~break:(fun (_, a) (_, b) -> a <> b)
    |! List.map ~f:(fun l ->
      p (Option.(value ~default:(pcdata "No Project: ")
                   (List.hd l >>= fun (_, p) ->
                    p >>= fun p -> return (b [pcdataf "%s: " p])))
         ::
           (List.map l (fun (n, p) ->
             Template.a_link Services.libraries
               [pcdata n] ([`basic;`fastq], [qname (n, p)]))
             |! interleave_list ~sep:(pcdata ", ")))) in
  let cell =
    paragraphs @ [
      if List.length libs > 1 then
        p [
          pcdata " (";
          Template.a_link Services.libraries [pcdata "all"]
            ([`basic; `fastq], List.map ~f:qname libs);
          pcdata ")"
        ]
      else
        span []
    ]
  in
  (sortability, cell)

let get_flowcell_by_serial_name ~dbh serial_name =
  Access.Flowcell.get_all ~dbh
  >>| List.filter ~f:(fun f ->
    Layout.Record_flowcell.(f.g_value.serial_name) = serial_name)
  >>= fun search ->
  match search with
  | [ one ] -> return one
  | [] -> error (`no_flowcell_named serial_name)
  | more ->
    error (`more_than_one_flowcell_called serial_name)

let flowcell_lanes_table hsc ~serial_name =
  with_database hsc (fun ~dbh ->
    get_flowcell_by_serial_name ~dbh serial_name >>= fun one ->
    let lanes =
      let lane = ref 0 in
      Layout.Record_flowcell.(
        return one.g_value
        >>= fun {serial_name; lanes} ->
        while_sequential (Array.to_list lanes) (fun lane_t ->
          incr lane;
          Layout.Record_lane.(
            Access.Lane.get ~dbh lane_t >>| (fun l -> l.g_value)
            >>= fun {
  	      seeding_concentration_pM ; total_volume ;
  	      libraries ; pooled_percentages ;
  	      requested_read_length_1 ; requested_read_length_2 ;
  	      contacts ; } ->
            let people = Array.to_list contacts in
            Authentication.authorizes (`view (`lane_of people))
                                                      >>= fun authorization ->
            if authorization
            then begin
              while_sequential people (Persons_service.person_essentials dbh)
              >>= fun people ->
              while_sequential
                (Array.to_list (Array.mapi libraries ~f:(fun i a -> (i,a))))
                (fun (i, ilibt) ->
                  Layout.Record_input_library.(
                    Access.Input_library.get ~dbh ilibt >>| (fun f -> f.g_value)
                    >>= fun { library; _ } ->
                    Layout.Record_stock_library.(
                      Access.Stock_library.get ~dbh library  >>| (fun f -> f.g_value)
                      >>= fun { name; project; _ } ->
                      return (name, project))))
              >>= fun libs ->
              return Html5.(
                let pcf = (ksprintf pcdata "%.0f") in
                let of_float_opt o =
                  Option.value_map o ~default:(`sortable ("", [pcdata ""]))
                    ~f:(fun f ->
                        `sortable (Float.to_string f, [pcf f])) in
                  [
                    `sortable (Int.to_string !lane,
                               [ksprintf pcdata "Lane %d" !lane]);
                    of_float_opt seeding_concentration_pM;
		    of_float_opt total_volume;
		    `sortable (sortable_people_cell people);
                    `sortable (sortable_libraries_cell libs);
                  ])
              end
              else
                return [])))
      in
      lanes >>= fun lanes ->
      return Template.(Html5.(
        content_section
          (ksprintf pcdata "Lanes of %s" serial_name)
          (content_table
             ([ `head_cell Msg.lane;
	        `head_cell Msg.seeding_concentration;
	        `head_cell Msg.volume;
	        `head_cell Msg.contacts_of_lane;
	        `head_cell Msg.libraries_of_lane; ]
              :: lanes)))))

let get_clusters_info ~configuration path =
  let make file =
    Data_access.File_cache.get_clusters_info file >>= fun ci_om ->
    let open Html5 in
    let open Template in
    let column_names = [
      "Lane"; "clusters_raw"; "clusters_raw_sd"; "clusters_pf";
      "clusters_pf_sd"; "prc_pf_clusters"; "prc_pf_clusters_sd"; ] in
    let first_row = List.map column_names (fun s -> `head [pcdata s]) in
    let other_rows =
      List.mapi (Array.to_list ci_om) (fun i cio ->
        let open Hitscore_interfaces.Hiseq_raw_information in
        let f g =
          Option.(value_map ~default:"" ~f:(sprintf "%.2f") (map cio ~f:g)) in
        let r s = [div ~a:[ a_style "text-align:right"] [codef "%s" s]] in
        [ `sortable (Int.to_string (i + 1), [codef "%d" (i + 1)]);
          (let s = f (fun x -> x.clusters_raw      ) in `sortable (s, r s));
          (let s = f (fun x -> x.clusters_raw_sd   ) in `sortable (s, r s));
          (let s = f (fun x -> x.clusters_pf       ) in `sortable (s, r s));
          (let s = f (fun x -> x.clusters_pf_sd    ) in `sortable (s, r s));
          (let s = f (fun x -> x.prc_pf_clusters   ) in `sortable (s, r s));
          (let s = f (fun x -> x.prc_pf_clusters_sd) in `sortable (s, r s)); ])
    in
    return (content_table (first_row :: other_rows))
  in
  let xml_read1 = Filename.concat path "Data/reports/Summary/read1.xml" in
  make xml_read1

let hiseq_raw_info ~configuration ~serial_name =
  let open Html5 in
  let open Template in
  with_database ~configuration (fun ~dbh ->
    let layout = Classy.make dbh in
    layout#hiseq_raw#all >>| List.filter ~f:(fun h -> h#flowcell_name = serial_name)
    >>= fun dirs ->
    while_sequential dirs ~f:(fun d ->
      let m = Common.check_hiseq_raw_availability ~dbh ~hiseq_raw:d#g_pointer in
      double_bind m
        ~error:(fun e ->
          match e with
          | `hiseq_dir_deleted -> return false
          | `Layout _ as e -> error e)
        ~ok:(fun _ -> return true)
      >>= fun available ->
      if available then (
        Common.hiseq_raw_full_path ~configuration d#hiseq_dir_name
        >>= fun hsdata_path ->
        double_bind (get_clusters_info ~configuration hsdata_path)
          ~ok:(fun tab ->
            return (content_section (pcdataf "Clusters Info") tab))
          ~error:(fun e ->
            return
              (content_section (pcdataf "Clusters Info")
                 (content_paragraph [strong [codef "not available"]])))
        >>= fun cluster_stats_subsection ->
        let intro_paragraph =
          content_paragraph [
            ul [
              li [ strong [pcdataf "Host:" ]; pcdataf " %s" d#host; ];
              li [ strong [pcdataf "Path: "]; codef "%s" d#hiseq_dir_name];
              li [ strong [pcdataf "Run-type: "];
                   pcdataf "%d%s%s."
                     d#read_length_1
                     (Option.value_map ~default:""
                        ~f:(sprintf " x %d") d#read_length_index)
                     (Option.value_map ~default:""
                        ~f:(sprintf " x %d") d#read_length_2) ];
              li [ strong [pcdataf "With Intensities: "];
                   codef "%b" d#with_intensities ];
            ]] in
        let section =
          content_section
            (pcdataf "%s Run" (d#run_date |! Time.to_local_date |! Date.to_string))
            (content_list [intro_paragraph; cluster_stats_subsection]) in
        return (Some section))
      else
        return None)
    >>= fun l ->
    return
      (content_section (pcdataf "Hiseq-raw director%s"
                          (if List.length l = 1 then "y" else "ies"))
         (content_list (List.filter_opt l))))

type demux_stats_filter =
[`none | `barcoded of int * string | `non_barcoded of int ]

let apply_demux_stats_filter
    ~filter ~lane_index ~library_name ~only_one_in_the_lane ~f =
  match filter with
  | `none -> Some (f ())
  | `barcoded (lane, libname)
      when lane = lane_index && libname = library_name -> Some (f ())
  | `non_barcoded lane when
      lane = lane_index && (
        library_name = sprintf "lane%d" lane
        || library_name = sprintf "UndeterminedLane%d" lane
          || only_one_in_the_lane) -> Some (f ())
  | _ -> None

let get_demux_stats ?(filter:demux_stats_filter=`none) ~configuration path =
    (* eprintf "demux: %s\n%!" dmux_sum; *)
  let make dmux_sum =
    Data_access.File_cache.get_demux_summary dmux_sum >>= fun ls_la ->
    let open Html5 in
    let open Template in
    let first_row = [
      `head_cell Msg.lane;
      `head_cell Msg.library_name;
      `head_cell Msg.number_of_reads;
      `head_cell Msg.zero_mismatch;
      `head_cell Msg.percent_bases_over_q30;
      `head_cell Msg.mean_qs] in
    let other_rows =
      List.mapi (Array.to_list ls_la) (fun i ls_l ->
        let only_one_in_the_lane = List.length ls_l = 1 in
        List.filter_map ls_l (fun ls ->
          let open Hitscore_interfaces.B2F_unaligned_information in
          let nb2 f = `number (sprintf "%.2f", f) in
          let nb0 f = `number (sprintf "%.0f", f) in
          let make_row () =
            let name =
              if ls.name = sprintf "lane%d" (i + 1)
              then sprintf "Undetermined Lane %d" (i + 1)
              else ls.name in
            [
              `sortable (Int.to_string (i + 1), [codef "%d" (i + 1)]);
              `sortable (name, [ pcdata name ]);
              nb0 ls.cluster_count;
              nb2 (100. *. ls.cluster_count_m0 /. ls.cluster_count);
              nb2 (100. *. ls.yield_q30 /. ls.yield);
              nb2 (ls.quality_score_sum /. ls.yield);
              (*   s ls.yield; s ls.yield_q30; s ls.cluster_count;
                   s ls.cluster_count_m0; s ls.cluster_count_m1;
                   s ls.quality_score_sum; *)
            ] in
          apply_demux_stats_filter
            ~filter ~lane_index:(i + 1) ~library_name:ls.name
            ~only_one_in_the_lane ~f:make_row))
    in
    return (first_row, List.concat other_rows)
  in
  let dmux_sum = Filename.concat path "Flowcell_demux_summary.xml" in
  make dmux_sum

let basecall_stats_path_of_unaligned ~configuration ~dbh directory serial_name =
  Common.all_paths_of_volume ~configuration ~dbh directory
  >>= (function
  | [one] -> return one
  | _ -> error (`wrong_unaligned_volume directory))
  >>= fun unaligned_path ->
  return
    (Filename.concat unaligned_path (sprintf "Basecall_Stats_%s" serial_name))

let demux_info ~configuration ~serial_name =
  let open Template in
  let open Html5 in
  with_database ~configuration (fun ~dbh ->
    let layout = Classy.make dbh in
    layout#bcl_to_fastq#all >>| List.filter ~f:(fun b -> b#g_status = `Succeeded)
    >>= fun successes ->
    while_sequential successes ~f:(fun b2f_eval ->
      b2f_eval#raw_data#get >>| (fun x -> x#flowcell_name)
      >>= fun b2f_fcid ->
      if b2f_fcid <> serial_name then
        return None
      else
        begin
          begin match b2f_eval#g_result with
          | None ->
            error (`bcl_to_fastq_succeeded_without_result b2f_eval#g_pointer)
          | Some r ->
            r#get >>| (fun x -> x#directory)
            >>= fun directory ->
            basecall_stats_path_of_unaligned ~configuration ~dbh
              directory#pointer serial_name
          end
          >>= fun stat_path ->
          double_bind (get_demux_stats ~configuration stat_path)
            ~ok:(fun (h, rs) ->
              let tab = content_table (h :: rs) in
              return (content_section (pcdataf "Stats") tab))
            ~error:(fun e ->
              return
                (content_section (pcdataf "Stats Not Available")
                   (content_paragraph [])))
          >>= fun stats ->
          let optmap f x = Option.value_map ~default:"—" ~f x in
          let opt x = Option.value ~default:"—" x in
          let title = codef "Bcl_to_fastq %d" b2f_eval#g_id in
          let intro = content_paragraph [
            pcdata "Ran from ";
            strong [codef "%s" (optmap Time.to_string b2f_eval#g_started)];
            pcdata " to ";
            strong [codef "%s" (optmap Time.to_string b2f_eval#g_completed)];
            pcdata ".";
            br ();
            strong [pcdataf "Mismatch: "]; pcdataf "%d, " b2f_eval#mismatch;
            strong [pcdataf "Version: "]; pcdataf "%s, " b2f_eval#version;
            strong [pcdataf "Tiles: "]; pcdataf "%s, " (opt b2f_eval#tiles);
            strong [pcdataf "Bases-Mask: "]; pcdataf "%s, "
              (opt b2f_eval#bases_mask);
          ] in
          let section =
            content_section title (content_list (intro :: stats :: [])) in
          return (Some section)
        end
    )
    >>| List.filter_opt >>= fun b2f_evals ->
    let title =
      match b2f_evals with
      | [] -> "Never Demultiplexed"
      | [one] -> "Demultiplexing"
      | more -> "Demultiplexings" in
    return (content_section (pcdata title)
              (content_list b2f_evals)))

let make configuration =
  let open Template in
  (fun (serial_name) () ->
    let main_title = (sprintf "FC:%s" serial_name) in
    Template.default ~title:main_title
      (Authentication.authorizes (`view `flowcell)
       >>= function
       | true ->
         flowcell_lanes_table ~serial_name configuration
         >>= fun tab_section ->
         Authentication.authorizes (`view `hiseq_raw_info)
         >>= fun hiseq_raw_authorization ->
         begin if hiseq_raw_authorization then
             hiseq_raw_info ~configuration ~serial_name
           else
             return (content_paragraph [])
         end
         >>= fun hr_section ->
         Authentication.authorizes (`view `demux_info)
         >>= fun demux_info_auth ->
         begin if demux_info_auth then
             demux_info ~configuration ~serial_name
           else
             return (content_paragraph [])
         end
         >>= fun demux_info ->
         let content =
           return (content_list [tab_section; hr_section; demux_info]) in
         make_content ~configuration ~main_title content
       | false ->
         Template.make_authentication_error ~configuration ~main_title
           (return [Html5.pcdataf
                       "You may not view the flowcell called %s."
                       serial_name])))

