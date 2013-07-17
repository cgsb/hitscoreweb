
open Hitscoreweb_std
module Web_data_access = Hitscoreweb_data_access
module Authentication = Hitscoreweb_authentication
module Template = Hitscoreweb_template
module Services = Hitscoreweb_services
module Persons_service = Hitscoreweb_persons


let sortable_people_cell people =
  let open Html5 in
  let sortability =
    List.map people (fun (_, _, x) -> x) |> String.concat ~sep:", " in
  let cell =
    (List.map people (fun (f, l, e) ->
      [
        Template.a_link Services.persons
          [ksprintf Html5.pcdata "%s %s" f l] (Some true, [e]);
        br () ]) |> List.concat)
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
    |> List.group ~break:(fun (_, a) (_, b) -> a <> b)
    |> List.map ~f:(fun l ->
      p (Option.(value ~default:(pcdata "No Project: ")
                   (List.hd l >>= fun (_, p) ->
                    p >>= fun p -> return (b [pcdataf "%s: " p])))
         ::
           (List.map l (fun (n, p) ->
             Template.a_link Services.libraries
               [pcdata n] ([`basic;`fastq], [qname (n, p)]))
             |> interleave_list ~sep:(pcdata ", ")))) in
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

let email_content ~flowcell_name =
  sprintf "Dear GenCore Users,\n\
           \n\
           Results for your recently sequenced \
           libraries on flowcell %s are now available here:\n\
           \n\
           https://gencore.bio.nyu.edu/hiseq_runs\n\
           \n\
           Click on the links for each library to see additional details such as \
           quality statistics and paths to the fastq files on the Bowery \
           cluster.\n\
           \n\
           Please let us know if you have questions.\n\
           \n\
           Best,\n\
           GenCore Team" flowcell_name

(* A section with the invoicing information and delivery draft emails. *)
let invoicing_section ~serial_name configuration =
  let open Html5 in
  let open Template in
  with_database configuration (fun ~dbh ->
    let layout = Classy.make dbh in
    layout#flowcell#all >>= fun all_flowcells ->
    layout#invoicing#all >>= fun all_invoices ->
    layout#lane#all >>= fun all_lanes ->
    layout#person#all >>= fun all_persons ->
    let this_flowcell =
      List.find all_flowcells (fun fc -> fc#serial_name = serial_name) in
    let lanes =
      match this_flowcell with
      | None -> [] (* if the flowcell is not found an error should
                      have already happened in the previous section *)
      | Some f ->
        List.filter_map all_lanes (fun l ->
          match Array.findi f#lanes (fun _ fl -> fl#id = l#g_id) with
          | Some (idx, _) -> Some (idx, l)
          | None -> None)
    in
    let invoices =
      List.filter_map all_invoices (fun invoice ->
        let idx_lanes =
          List.filter lanes (fun (idx, lane) ->
            Array.exists invoice#lanes (fun il -> il#id = lane#g_id)) in
        match idx_lanes with
        | [] -> None
        | idx_lanes ->
          let pi =
            List.find all_persons (fun p -> p#g_id = invoice#pi#id) in
          let pi_full_name =
            Option.value_map ~default:(strongf "NOT FOUND !!!") pi
              ~f:(fun p -> strongf "%s, %s" p#family_name p#given_name) in
          (* let pi_name = *)
          (* Option.value_map ~default:( "NOT FOUND !!!") pi *)
          (* ~f:(fun p -> p#family_name) in *)
          let opt = Option.value ~default:"—" in
          let contacts =
            List.map idx_lanes (fun (_, lane) ->
              List.filter all_persons (fun p ->
                Array.exists lane#contacts (fun c -> c#id = p #g_id)))
            |> List.concat
            |> List.dedup ~compare:(fun a b -> Int.compare a#g_id b#g_id) in
          let email_contact_list =
            List.map contacts (fun c ->
              sprintf "\"%s, %s\" <%s>" c#family_name c#given_name c#email) in
          let email_content =
            email_content ~flowcell_name:serial_name in
          let span_email, div_email =
            hide_show_div ~start_hidden:true
              ~show_message:"Show delivery draft email"
              ~hide_message:"Hide delivery draft email"
              [p [pcdataf "To: %s" (String.concat ~sep:", " email_contact_list)];
               p [pcdataf "Subject: Data for %s" serial_name];
               pre [pcdata email_content]] in
          let gmail_link =
            let params = Ocsigen_lib.Url.make_encoded_parameters [
                ("to", (String.concat ~sep:", " email_contact_list));
                ("su", sprintf "Data for %s" serial_name);
                ("body", email_content);
              ] in
            sprintf "https://mail.google.com/mail/?view=cm&fs=1&tf=1&%s" params
          in
          Some (content_paragraph [
              strongf "Invoice";
              pcdataf " (%d) to " invoice#g_id;
              pi_full_name;
              ul [
                li [pcdataf
                      "Account: %s, Fund: %s, Org: %s, Program: %s, Project: %s"
                      (opt invoice#account_number) (opt invoice#fund)
                      (opt invoice#org) (opt invoice#program)
                      (opt invoice#project)];
                li [pcdataf "Takes %02.0f%% of lane%s %s"
                      invoice#percentage
                      (if List.length idx_lanes > 1 then "s" else "")
                      (List.map idx_lanes (fun (i, _) -> sprintf "%d" (i + 1))
                       |> String.concat ~sep:", ")];
                li [
                  pretty_box [span_email; div_email];
                  core_a ~a:[ a_hreff "%s" gmail_link; a_target "_blank"]
                    [pcdata "Open in Gmail-Compose"];
                ];
              ];
            ])) in
    return (content_list invoices))
  >>= fun content ->
  return (content_section (pcdata "Invoicing") content)


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
            (pcdataf "%s Run" (d#run_date |> Time.to_local_date |> Date.to_string))
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

(*doc
`get_demux_stats` returns two tables: the library stats table and the
lane stats table.

The lane-stats are computed while going through the libraries.

The tables are returned as a tuple:
`(libraries_first_row, libraries_other_rows, lane_first_row, lane_other_rows)`.

*)
let get_demux_stats ?(filter:demux_stats_filter=`none) ~configuration path =
    (* eprintf "demux: %s\n%!" dmux_sum; *)
  let make dmux_sum =
    Data_access.File_cache.get_demux_summary dmux_sum >>= fun ls_la ->
    let open Html5 in
    let open Template in
    let nb2 f = `number (sprintf "%.2f", f) in
    let nb0 f = `number (sprintf "%.0f", f) in
    let lane_stats = ref [] in
    (* \-> list of
       lane-nb × (clust-count × yield_q30 × yield × qscore_sum × undetermined)*)
    let libraries_first_row = [
      `head_cell Msg.lane;
      `head_cell Msg.library_name;
      `head_cell Msg.number_of_reads;
      `head_cell Msg.zero_mismatch;
      `head_cell Msg.percent_bases_over_q30;
      `head_cell Msg.mean_qs] in
    let libraries_other_rows =
      List.mapi (Array.to_list ls_la) (fun i ls_l ->
        let only_one_in_the_lane = List.length ls_l = 1 in
        let lane_clust_count = ref 0. in
        let lane_yield_q30 = ref 0. in
        let lane_yield = ref 0. in
        let lane_qscore = ref 0. in
        let lane_undetermined = ref 0. in
        let rows =
          List.filter_map ls_l (fun ls ->
              let open Hitscore_interfaces.B2F_unaligned_information in
              lane_clust_count := !lane_clust_count +. ls.cluster_count;
              lane_yield_q30 := !lane_yield_q30 +. ls.yield_q30;
              lane_yield := !lane_yield +. ls.yield;
              lane_qscore := !lane_qscore +. ls.quality_score_sum;
              let make_row () =
                let name =
                  if (ls.name = sprintf "lane%d" (i + 1)
                     || ls.name = sprintf "UndeterminedLane%d" (i + 1))
                  then (
                    lane_undetermined := ls.cluster_count;
                    sprintf "Undetermined Lane %d" (i + 1)
                  ) else ls.name in
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
                ~only_one_in_the_lane ~f:make_row) in
        lane_stats := (i, (!lane_clust_count, !lane_yield_q30,
                           !lane_yield, !lane_qscore, !lane_undetermined))
                      :: !lane_stats;
        rows)
    in
    let lane_first_row = [
      `head_cell Msg.lane;
      `head_cell Msg.number_of_reads;
      `head_cell Msg.number_of_undetermined_reads;
      `head_cell Msg.percent_bases_over_q30;
      `head_cell Msg.mean_qs] in
    let lane_other_rows =
      List.map (List.sort !lane_stats ~cmp:(fun (i, _) (j,_) -> Int.compare i j))
        ~f:(fun (lane_index, (clusters, yq30, y, qs, undetermined)) ->
            [`sortable (Int.to_string (lane_index + 1),
                        [codef "%d" (lane_index + 1)]);
             nb0 clusters;
             nb0 undetermined;
             (* nb2 (100. *. ls.cluster_count_m0 /. ls.cluster_count); *)
             nb2 (100. *. yq30 /. y);
             nb2 (qs /. y); ]) in
    return (libraries_first_row, List.concat libraries_other_rows,
            lane_first_row, lane_other_rows)
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
            ~ok:(fun (head_libs, rows_libs, head_lanes, rows_lanes) ->
              let libs_tab = content_table (head_libs :: rows_libs) in
              let lanes_tab = content_table (head_lanes :: rows_lanes) in
              return (content_list [
                  content_section (pcdataf "Library Stats") libs_tab;
                  content_section (pcdataf "Lane Stats") lanes_tab;
                ]))
            ~error:(fun e ->
              return
                (content_section (pcdataf "Stats Not Available")
                   (content_paragraph [])))
          >>= fun stats ->
          let optmap f x = Option.value_map ~default:"—" ~f x in
          let opt x = Option.value ~default:"—" x in
          let title =
            let t = Re_posix.re ".*Bustard.*" in
            let re = Re.compile t in
            let is_olb = Re.execp re b2f_eval#basecalls_path in
            codef "Bcl_to_fastq %d %s" b2f_eval#g_id
              (if is_olb then "(With OLB)" else "")
          in
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
              (opt b2f_eval#bases_mask); br ();
            strong [pcdataf "Basecalls-Path: "];
            codef "%s" b2f_eval#basecalls_path;
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
         invoicing_section ~serial_name configuration
         >>= fun invoicing_sec ->
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
           return (content_list [tab_section; invoicing_sec;
                                 hr_section; demux_info]) in
         make_content ~configuration ~main_title content
       | false ->
         Template.make_authentication_error ~configuration ~main_title
           (return [Html5.pcdataf
                       "You may not view the flowcell called %s."
                       serial_name])))
