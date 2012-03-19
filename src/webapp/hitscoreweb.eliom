open Hitscoreweb_std

module Queries = Hitscoreweb_queries

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

module Layout_service = Hitscoreweb_layout_service

    
module Cache = struct

  module String_map = Map.Make(String)

  let _run_param_cache =
    ref (String_map.empty:
           Hitscore_interfaces.Hiseq_raw_information.clusters_info option array
           String_map.t)
      
  let get_clusters_info path =
    let make file = 
      read_file file >>= fun xml_s ->
      let xml =
        Xml_tree.(in_tree (make_input (`String (0, xml_s)))) in
      Hitscore_lwt.Hiseq_raw.clusters_summary (snd xml) |! of_result
    in
    match String_map.find !_run_param_cache path with
    | Some r -> return r
    | None ->
      make path >>= fun res ->
      _run_param_cache := String_map.add ~key:path ~data:res !_run_param_cache;
      return res
        
  let _demux_summary_cache =
    ref (String_map.empty:
           Hitscore_interfaces.B2F_unaligned_information.demux_summary String_map.t)
      
  let get_demux_summary path =
    let make file = 
      read_file file >>= fun xml_s ->
      let xml =
        Xml_tree.(in_tree (make_input (`String (0, xml_s)))) in
      Hitscore_lwt.B2F_unaligned.flowcell_demux_summary (snd xml)
      |! of_result
    in
    match String_map.find !_demux_summary_cache path with
    | Some r -> return r
    | None ->
      make path >>= fun res ->
      _demux_summary_cache :=
        String_map.add ~key:path ~data:res !_demux_summary_cache;
      return res
        
end

module Persons_service = struct
  let person_essentials dbh person_t =
    Layout.Record_person.(
      get ~dbh person_t
      >>= fun { given_name; family_name; email; _ } ->
      return (given_name, family_name, email))

  let person_link ?(style=`full_name) dbh person_t =
    person_essentials dbh person_t >>= fun (f, l, e) ->
    let content =
      match style with
      | `full_name -> ksprintf Html5.pcdata "%s %s" f l
      | `family_name -> ksprintf Html5.pcdata "%s" l in
    return (Template.a_link Services.persons [content] (Some true, [e]))


  let persons ~full_view ?(transpose=false) ?(highlight=[]) hsc =
    Hitscore_lwt.db_connect hsc
    >>= fun dbh ->
    Layout.Record_person.(
      get_all ~dbh >>= fun plist ->
      let rows_m =
        of_list_sequential plist (fun p ->
          get ~dbh p
          >>= fun {print_name; given_name; middle_name; family_name;
  	           email; secondary_emails; roles; login; nickname; note;} ->
          let opt f m = Option.value_map ~f ~default:(f "") m in
          let is_vip = List.exists highlight ((=) email) in
          if not is_vip && (highlight <> []) then
            return None
          else Html5.(
            let email_field =
              let style = if is_vip then "color: green" else "" in
              `sortable (email, [code ~a:[a_id email; a_style style]
                                    [pcdata email]])
            in
            let text s = `sortable (s, [pcdata s]) in
            let opttext o = opt text o in
            let default = [
              opttext print_name;
              text given_name;
              opttext middle_name;
              text family_name;
              opttext nickname;
              email_field;
              `text (array_to_list_intermap secondary_emails ~sep:(pcdata ", ")
                       ~f:(codef "%s\n"));
              opttext login;
            ] in
            let supplement = 
              if not full_view then [] else [
                `text (array_to_list_intermap roles ~sep:(br ())
                         ~f:(fun s -> pcdataf "%s" 
                           (Layout.Enumeration_role.to_string s)));
                opttext note;]
            in
            return (Some (default @ supplement)))) in
      rows_m >>= fun rows ->
      Hitscore_lwt.db_disconnect hsc dbh
      >>= fun _ ->
      let actual_rows = List.filter_opt rows in
      let nrows = List.length actual_rows in
      return Template.(Html5.(
        let normal_rows = [
          `head [pcdata "Print name"];
	  `head [pcdata "Given name"];
	  `head [pcdata "Middle name"];
	  `head [pcdata "Family name"];
	  `head [pcdata "Nickname"];
	  `head [pcdata "Email"];
          `head [pcdata "Secondary Emails"];
	  `head [pcdata "Login"]] in
        let supplement = 
          if not full_view then [] else [
	    `head [pcdata "Roles"];
	    `head [pcdata "Note"];] in
        content_section 
          (ksprintf pcdata "Found %d Person%s" nrows
             (if nrows > 1 then "s" else ""))
          (content_table ~transpose
             ((normal_rows @ supplement)
              :: actual_rows)))))

  let make configuration =
    (fun (transpose, highlight) () ->
      Template.default ~title:"Persons"
        (Authentication.authorizes (`view `persons)
         >>= function
         | true ->
           Authentication.authorizes (`view `full_persons)
           >>= fun full_view ->
           Template.make_content ~configuration
             ~main_title:"People" 
             (persons ?transpose ~highlight ~full_view configuration)
         | false ->
           Template.make_authentication_error ~configuration
             ~main_title:"Persons" 
             (return [Html5.pcdataf "You may not view any person."])))

end

module Flowcell_service = struct
    

  let sortable_people_cell people =
    let open Html5 in
    let sortability =
      List.map people trd3 |! String.concat ~sep:", " in
    let cell = 
      (List.map people (fun (f, l, e) ->
        [ 
          Template.a_link Services.persons
            [ksprintf Html5.pcdata "%s %s" f l] (Some true, [e]);
          br () ]) |! List.flatten)
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
    let qnames =
      List.map libs (function (l, None) -> l
      | (l, Some p) -> sprintf "%s.%s" p l) in
    let sortability = String.concat ~sep:", " qnames in
    let cell =
      (List.map qnames (fun qn ->
        Template.a_link Services.libraries [pcdata qn] ([`basic], [qn]))
        |! interleave_list ~sep:(pcdata ", "))
      @ [
        if List.length qnames > 1 then
          small [
            pcdata " (";
            Template.a_link Services.libraries [pcdata "all"] ([`basic], qnames);
            pcdata ")"
          ]
        else
          span []
      ]
    in
    (sortability, cell)

  let get_flowcell_by_serial_name ~dbh serial_name = 
    Layout.Search.record_flowcell_by_serial_name ~dbh serial_name
    >>= function
    | [ one ] -> return one
    | [] -> error (`no_flowcell_named serial_name)
    | more ->
      error (`layout_inconsistency (`record_flowcell, 
                                    `more_than_one_flowcell_called serial_name))
      
  let flowcell_lanes_table hsc ~serial_name =
    Hitscore_lwt.with_database hsc (fun ~dbh ->
      get_flowcell_by_serial_name ~dbh serial_name >>= fun one ->
      let lanes =
        let lane = ref 0 in
        Layout.Record_flowcell.(
          get ~dbh one >>= fun {serial_name; lanes} ->
          of_list_sequential (Array.to_list lanes) (fun lane_t ->
            incr lane;
            Layout.Record_lane.(
              get ~dbh lane_t
              >>= fun {
  	        seeding_concentration_pM ;
  	        total_volume ;
  	        libraries ;
  	        pooled_percentages ;
  	        requested_read_length_1 ;
  	        requested_read_length_2 ;
  	        contacts ; } ->
              of_list_sequential (Array.to_list contacts) 
                (Persons_service.person_essentials dbh)
              >>= fun people ->
              of_list_sequential
                (Array.to_list (Array.mapi libraries ~f:(fun i a -> (i,a))))
                (fun (i, ilibt) ->
                  Layout.Record_input_library.(
                    get ~dbh ilibt
                    >>= fun { library; _ } ->
                    Layout.Record_stock_library.(
                      get ~dbh library
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
                ]))))
      in
      lanes >>= fun lanes ->
      return Template.(Html5.(
        content_section 
          (ksprintf pcdata "Lanes of %s" serial_name)
          (content_table 
             ([ `head [pcdata "Lane Nb"]; 
	        `head [pcdata "Seeding C."];
	        `head [pcdata "Vol."];
	        `head [pcdata "Contacts"];
	        `head [pcdata "Libraries"];]
              :: lanes)))))

  let get_clusters_info ~configuration path =
    let make file = 
      Cache.get_clusters_info file >>= fun ci_om ->
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
    Hitscore_lwt.with_database ~configuration ~f:(fun ~dbh ->
      Layout.Search.record_hiseq_raw_by_flowcell_name ~dbh serial_name
      >>= fun dirs ->
      of_list_sequential dirs ~f:(fun d ->
        let m =
          Hitscore_lwt.Common.check_hiseq_raw_availability ~dbh ~hiseq_raw:d in
        double_bind m
          ~error:(fun e -> 
            match e with
            | `hiseq_dir_deleted -> return false
            |  `pg_exn _ | `io_exn _ | `layout_inconsistency (_, _)
            | `no_flowcell_named _ as e -> error e)
          ~ok:(fun _ -> return true)
        >>= fun available ->
        if available then 
          Layout.Record_hiseq_raw.(
            get ~dbh d
            >>= fun {read_length_1; read_length_index; read_length_2; 
                     run_date; host; hiseq_dir_name; with_intensities} ->
            Hitscore_lwt.Common.hiseq_raw_full_path ~configuration hiseq_dir_name
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
                  li [ strong [pcdataf "Host:" ]; pcdataf " %s" host; ];
                  li [ strong [pcdataf "Path: "]; codef "%s" hiseq_dir_name];
                  li [ strong [pcdataf "Run-type: "]; 
                       pcdataf "%ld%s%s."
                         read_length_1
                         (Option.value_map ~default:""
                            ~f:(sprintf " x %ld") read_length_index)
                         (Option.value_map ~default:""
                            ~f:(sprintf " x %ld") read_length_2) ];
                  li [ strong [pcdataf "With Intensities: "];
                       codef "%b" with_intensities ];
                ]] in
            let section =
              content_section
                (pcdataf "%s Run" (run_date |! Time.to_local_date |! Date.to_string))
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
    
  let get_demux_stats ?(filter:demux_stats_filter=`none) ~configuration path =
    (* eprintf "demux: %s\n%!" dmux_sum; *)
    let make dmux_sum =
      Cache.get_demux_summary dmux_sum >>= fun ls_la ->
      let open Html5 in
      let open Template in
      let column_names = [
        "Lane";
        "Lib ID";
        "# Reads";
        "% 0 mismatch";
        "% bases ≥ Q30";
        "Mean QS (PF)";
      ] in
      let first_row = List.map column_names (fun s -> `head [pcdata s]) in
      let other_rows =
        List.mapi (Array.to_list ls_la) (fun i ls_l ->
          List.filter_map ls_l (fun ls ->
            let open Hitscore_interfaces.B2F_unaligned_information in
            let nb2 f = `number (sprintf "%.2f", f) in 
            let nb0 f = `number (sprintf "%.0f", f) in 
            let make_row () =
              let name =
                if ls.name = sprintf "lane%d" (i + 1)
                then sprintf "Undetermined Lane %d" (i + 1)
                else ls.name in
              Some [
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
            match filter with
            | `none -> make_row ()
            | `barcoded (lane, libname) when lane = i + 1 && libname = ls.name ->
              make_row ()
            | `non_barcoded lane when
                lane = i + 1 && ls.name = sprintf "lane%d" lane ->
              make_row ()
            | _ -> None))
      in
      return (first_row, List.flatten other_rows)
    in
    let dmux_sum = Filename.concat path "Flowcell_demux_summary.xml" in
    make dmux_sum
      
  let basecall_stats_path_of_unaligned ~configuration ~dbh directory serial_name =
    Hitscore_lwt.Common.paths_of_volume ~configuration ~dbh directory
    >>= (function
    | [one] -> return one
    | _ -> error (`wrong_unaligned_volume directory))
    >>= fun unaligned_path ->
    return
      (Filename.concat unaligned_path (sprintf "Basecall_Stats_%s" serial_name))

  let demux_info ~configuration ~serial_name =
    let open Template in
    let open Html5 in
    Hitscore_lwt.with_database ~configuration ~f:(fun ~dbh ->
      Layout.Function_bcl_to_fastq.(
        get_all_succeeded ~dbh >>= fun successes ->
        of_list_sequential successes ~f:(fun b2f ->
          get ~dbh b2f >>= fun b2f_eval ->
          Layout.Record_hiseq_raw.(
            get ~dbh b2f_eval.raw_data >>| fun x -> x.flowcell_name)
          >>= fun b2f_fcid ->
          if b2f_fcid <> serial_name then
            return None
          else
            begin
            Layout.Record_bcl_to_fastq_unaligned.(
              match b2f_eval.g_result with
              | None -> error (`bcl_to_fastq_succeeded_without_result b2f)
              | Some r ->
                get ~dbh r >>= fun {directory} ->
                basecall_stats_path_of_unaligned ~configuration ~dbh
                  directory serial_name)
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
            let title = codef "Bcl_to_fastq %ld" b2f_eval.g_id in
            let intro = content_paragraph [
              pcdata "Ran from ";
              strong [codef "%s" (optmap Time.to_string b2f_eval.g_started)];
              pcdata " to ";
              strong [codef "%s" (optmap Time.to_string b2f_eval.g_completed)];
              pcdata ".";
              br ();
              strong [pcdataf "Mismatch: "]; pcdataf "%ld, " b2f_eval.mismatch;
              strong [pcdataf "Version: "]; pcdataf "%s, " b2f_eval.version;
              strong [pcdataf "Tiles: "]; pcdataf "%s, " (opt b2f_eval.tiles);
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
                  (content_list b2f_evals))))

  let make configuration =
    let open Template in
    (fun (serial_name) () ->
      let main_title = (sprintf "FC:%s" serial_name) in
      Template.default ~title:main_title
        (Authentication.authorizes (`view `full_flowcell)
         >>= function
         | true ->
           flowcell_lanes_table ~serial_name configuration
           >>= fun tab_section ->
           hiseq_raw_info ~configuration ~serial_name
           >>= fun hr_section ->
           demux_info ~configuration ~serial_name >>= fun demux_info ->
           let content =
             return (content_list [tab_section; hr_section; demux_info]) in
           make_content ~configuration ~main_title content
         | false ->
           Template.make_authentication_error ~configuration ~main_title
             (return [Html5.pcdataf 
                         "You may not view the flowcell called %s."
                         serial_name])))


end

module Libraries_service = struct

  (* Helper functions for type Services.libraries_show *)
  let show_list ~showing show l =
    if List.exists showing ((=) show) then l else []
  let show_list_fun ~showing show f =
    if List.exists showing ((=) show) then f () else []
  let show_list_m ~showing show f =
    if List.exists showing ((=) show) then f () else return []
    
  let barcodes_cell ~dbh bartype barcodes bartoms =
    let open Html5 in
    let barcodes_list =
      String.concat ~sep:"," 
        (List.map ~f:(sprintf "%ld") 
           (Array.to_list (Option.value ~default:[| |] barcodes))) in
    let custom_barcodes =
      match bartoms with
      | None | Some [| |] -> return [pcdata ""]
      | Some a -> 
        let l = Array.to_list a in
        Layout.Record_custom_barcode.(
          of_list_sequential l (fun id ->
            get ~dbh (unsafe_cast id)
            >>= fun {position_in_r1; position_in_r2; 
                     position_in_index; sequence} ->
            return [ br ();
                     ksprintf pcdata "%s(%s)"
                       sequence
                       (["R1", position_in_r1; 
                         "I", position_in_index; "R2", position_in_r2]
                        |! List.filter_map ~f:(function
                          | _, None -> None
                          | t, Some i -> Some (sprintf "%s:%ld" t i))
                                       |! String.concat ~sep:",")]))
        >>= fun pcdatas ->
        return (List.flatten pcdatas)
    in
    custom_barcodes >>= fun custom ->
    let non_custom =
      Option.value_map bartype
        ~default:(pcdata "NO BARCODE TYPE!") ~f:(fun t ->
          match Layout.Enumeration_barcode_provider.of_string t with
          | Ok `none -> pcdata ""
          | Ok `bioo -> ksprintf pcdata "BIOO[%s]" barcodes_list
          | Ok `bioo_96 -> ksprintf pcdata "BIOO-96[%s]" barcodes_list
          | Ok `illumina -> ksprintf pcdata "ILLUMINA[%s]" barcodes_list
          | Ok `nugen -> ksprintf pcdata "NUGEN[%s]" barcodes_list
          | Ok `custom -> strong [pcdata "CUSTOM"]
          | Error _ -> strong [pcdata "PARSING ERROR !!!"]
        )
    in
    return (sprintf "%s%s" (Option.value ~default:"" bartype) barcodes_list,
            non_custom :: custom)

  let fastq_files ~dbh ~configuration lib submissions =
    let open Html5 in
    let open Template in
    let  (idopt, libname, project, desc, app, stranded, truseq, rnaseq,
          bartype, barcodes, bartoms, p5, p7, note,
          sample_name, org_name, prep_email, protocol) = lib in
    let header_names =
      [ "Submission"; "# Reads"; "% 0 mismatch"; "% bases ≥ Q30"; "Mean QS (PF)"; ]
    in
    let h s = `head [pcdata s] in
    (* let sortable_text s = `sortable (s, [pcdata s]) in *)
    (* let sortable_fmt fmt = ksprintf sortable_text fmt in *)
    (* let none = `text [pcdata "—"] in *)
    let header = List.map header_names h in
    of_list_sequential submissions (fun (fcid, lane, contacts) ->
      Flowcell_service.get_flowcell_by_serial_name ~dbh fcid >>= fun fc_p ->
      let lane_pointer = Layout.Record_lane.unsafe_cast lane in
      Layout.Record_flowcell.(get ~dbh fc_p >>= fun {lanes} ->
                              return (Array.findi lanes ((=) lane_pointer)))
      >>= fun lane_index ->
      Queries.delivered_unaligned_directories_of_lane ~dbh lane_pointer
      >>= fun dirs ->
      let filter =
        let open Option in
        let or_none = value_map ~default:`none in
        or_none lane_index ~f:(fun i ->
          or_none libname ~f:(fun n -> 
            or_none bartype ~f:(fun t ->
              match Layout.Enumeration_barcode_provider.of_string t with
              | Ok `bioo | Ok `illumina -> `barcoded (i + 1, n)
              | _ -> `non_barcoded (i + 1))))
      in
      let lane_display =
        Option.value_map ~default:"??" lane_index
          ~f:(fun l -> sprintf "%s:L%d" fcid (l + 1)) in
      let first_cell =
        match filter with
        | `barcoded _ -> `sortable (lane_display, [pcdata lane_display])
        | _ ->
          `sortable (lane_display,
                     [pcdataf "Undetermined in %s" lane_display])
      in
      of_list_sequential dirs (fun dir ->
        Flowcell_service.basecall_stats_path_of_unaligned
          ~configuration ~dbh dir fcid
        >>= Flowcell_service.get_demux_stats ~configuration ~filter
        >>| snd
        >>| List.map ~f:(function
        | `sortable _ :: `sortable _ :: t -> first_cell :: t
        | any_row -> any_row)))
    >>| List.flatten 
    >>| List.flatten 
    >>= fun all_rows ->
    (* let all_rows = List.flatten rows_ll |! List.flatten in *)
    match all_rows with
    | [] -> return [ `sortable ("0", [pcdata "—"]) ]
    | l -> 
      return [`sortable (List.length l |! Int.to_string,
                         [content_table (header :: all_rows) |! html_of_content])]
(*
      let tmpcell = `text [ List.flatten rows_l
                           |! content_table |! html_of_content ] in

            double_bind (get_demux_stats ~configuration stat_path)
              ~ok:(fun tab ->
                return (content_section (pcdataf "Stats") tab))
              ~error:(fun e ->
                return
                  (content_section (pcdataf "Stats Not Available")
                     (content_paragraph [])))

  let get_demux_stats ~configuration path =
    *)
(*      let lane_display =
        Option.value_map ~default:"??" lane_index
          ~f:(fun l -> sprintf "L%d" (l + 1)) in
      return [ sortable_fmt "%s:%s" fcid lane_display; tmpcell; none; none; none ]
    ) 
    >>= fun rows ->
*)
      
  let submissions_cell submissions = 
    let open Html5 in
    let how_much =
      match List.length submissions with
      | 0 -> "Never" | 1 -> "Once: " | 2 -> "Twice: " 
      | n -> sprintf "%d times: " n in
    let flowcells = 
      List.map submissions fst3 |! List.dedup in
    let sortability = List.length submissions |! sprintf "%d" in
    let display =
      (ksprintf pcdata "%s" how_much)
      ::
        interleave_list ~sep:(pcdata ", ")
        (List.map flowcells (fun fcid ->
          let lanes = 
            List.filter submissions ~f:(fun (f,_,_) -> f = fcid)
            |! List.length in
          span [
            Template.a_link Services.flowcell [ksprintf pcdata "%s" fcid] fcid;
            ksprintf pcdata " (%d lane%s)"
              lanes (if lanes > 1 then "s" else "");
          ]))
      @ [pcdata "."]
    in
    return (sortability, display)
      
  let fetch_and_filter_libs ~dbh qualified_names =
    Queries.full_libraries dbh >>= fun library_list ->
    of_list_sequential library_list ~f:(fun result_item ->
      let  (idopt, name, project, desc, app, stranded, truseq, rnaseq,
            bartype, barcodes, bartoms, p5, p7, note,
            sample_name, org_name, prep_email, protocol) = result_item in
      let qualified_name_passes =
        let qualified_name =
          let opt = Option.value ~default:"" in
          sprintf "%s.%s" (opt project) (opt name) in
        (qualified_names = [] || List.exists qualified_names
            ~f:(fun qn -> qualified_name = qn || Some qn = name))
      in
      if not qualified_name_passes then return None
      else (
        let lib_id = Option.value ~default:0l idopt in
        Queries.library_submissions ~lib_id dbh
        >>= fun submissions ->
        let people = 
          submissions |! List.map ~f:(fun (_,_,ids) -> Array.to_list ids)
          |! List.flatten
          |! List.map ~f:(fun id -> Layout.Record_person.unsafe_cast id)
        in
        Authentication.authorizes (`view (`libraries_of people))
        >>= function
        | true -> return (Some (result_item, submissions))
        | false -> return None))

  let make_libs_table ~dbh ~configuration ~showing libs_filtered =
    let open Html5 in
    of_list_sequential (List.filter_opt libs_filtered)
      ~f:(fun ((idopt, name, project, desc, app, 
                stranded, truseq, rnaseq,
                bartype, barcodes, bartoms,
                p5, p7, note,
                sample_name, org_name,
                prep_email, protocol) as lib, submissions) ->
        submissions_cell submissions >>= fun submissions_cell ->
        barcodes_cell ~dbh bartype barcodes bartoms >>= fun barcoding ->
        let opt f o = Option.value_map ~default:(f "") ~f o in
        let person e =
          Template.a_link Services.persons
            [ksprintf Html5.pcdata "%s" e] (Some true, [e])
        in
        let text s = `sortable (s, [pcdata s]) in
        let opttext o = opt text o in
        let valopt = Option.value ~default:"" in
        let boolopt b =
          Option.value_map b ~default:(`sortable ("", []))
            ~f:(fun b ->
              `sortable (Bool.to_string b, [pcdataf "%b" b])) in
        let i32opt i32 =
          Option.value_map i32 ~default:(`sortable ("", []))
            ~f:(fun i ->
              `sortable (Int32.to_string i, [pcdataf "%ld" i])) in
        let mandatory_stuff = [opttext name; opttext project] in
        let basic_stuff =
          show_list ~showing `basic 
            [opttext desc; opttext sample_name; opttext org_name;
             `sortable submissions_cell; opttext app;]
        in
        let stock_stuff =
          show_list_fun ~showing `stock (fun () ->
            [`sortable barcoding; i32opt p5; i32opt p7; opttext protocol;
             boolopt stranded; boolopt truseq; opttext rnaseq;
             `sortable (valopt prep_email, [opt person prep_email]);
             opttext note; ])
        in
        show_list_m ~showing `fastq (fun () ->
          fastq_files ~configuration ~dbh lib submissions)
        >>= fun fastq_stuff ->
        return (mandatory_stuff @ basic_stuff @ stock_stuff @ fastq_stuff))
    >>= fun rows ->
    let mandatory_columns = [`head [pcdata "Name"]; `head [pcdata "Project"]] in
    let basic_columns = [ `head [pcdata "Description"];
      `head [pcdata "Sample-name"]; `head [pcdata "Organism"];
      `head [pcdata "Submitted"]; `head [pcdata "Application"];] in
    let stock_columns = [`head [pcdata "Barcoding"];
      `head [pcdata "P5 Adapter Length"]; `head [pcdata "P7 Adapter Length"];
      `head [pcdata "Protocol"]; `head [pcdata "Stranded"];
      `head [pcdata "Truseq-control"]; `head [pcdata "RNASeq-control"];
      `head [pcdata "Preparator"]; `head [pcdata "Note"];] in
    let header_row =
      mandatory_columns
      @ (show_list ~showing `basic basic_columns)
      @ (show_list ~showing `stock stock_columns)
      @ (show_list ~showing `fastq [`head [pcdata "FASTQ Files Information"]])
    in
    return (header_row :: rows)

  let intro_paragraph ~showing ~qualified_names =
    let open Html5 in
    let make_link (shwg, name) =
      Template.a_link Services.libraries [pcdata name]
        (shwg, qualified_names) in
    let links =
      List.map [ [`basic], "Basic"; [`fastq], "Fastq Info"; [`stock], "Stock Info";
                 [`basic;`stock; `fastq], "Full" ] make_link
      |! interleave_list ~sep:(pcdata ", ") in
    [pcdata "Choose view: ";] @ links @ [pcdata "."]
    
      
  let libraries ~showing ?(qualified_names=[]) ~configuration  =
    let open Html5 in
    Hitscore_lwt.with_database configuration (fun ~dbh ->
      fetch_and_filter_libs ~dbh qualified_names >>= fun libs_filtered ->
      make_libs_table ~dbh ~configuration ~showing libs_filtered)
    >>= fun main_table ->
    let nb_rows = List.length main_table in
    return Template.(
      content_section
        (ksprintf pcdata "Found %d librar%s:"
           nb_rows (if nb_rows = 2 then "y" else "ies"))
        (content_list [
          content_paragraph (intro_paragraph ~showing ~qualified_names);
          content_table ~transpose:(nb_rows = 2) main_table;
        ]))

  let make configuration =
    (fun (showing, qualified_names) () ->
      let main_title = "Libraries" in
      Template.default ~title:main_title
        (Authentication.authorizes (`view `libraries)
         >>= function
         | true ->
           Template.make_content ~configuration ~main_title    
             (libraries ~qualified_names ~showing ~configuration);
         | false ->
           Template.make_authentication_error ~configuration ~main_title
             (return [Html5.pcdataf "You may not view the libraries."])))

    
end

module Evaluations_service = struct


  let bcl_to_fastq_section ~dbh ~configuration name b2fs = 
    let open Html5 in
    let module B2F = Layout.Function_bcl_to_fastq in
    let module HSR = Layout.Record_hiseq_raw in
    let module FS = Layout.File_system in
    if List.length b2fs > 0 then (
      of_list_sequential b2fs ~f:(fun b2f ->
        B2F.get ~dbh b2f
        >>= fun {B2F.raw_data; availability; mismatch; version;
  	         tiles; sample_sheet; } ->
        HSR.get ~dbh raw_data
        >>= fun { HSR.flowcell_name; _ } ->
        Queries.sample_sheet_kind ~dbh sample_sheet
        >>= fun kind ->
        Layout.Record_sample_sheet.(
          get ~dbh sample_sheet >>= fun {file; _} ->
          FS.get_volume ~dbh file >>= fun vol ->
          IO.return (FS.volume_trees vol) >>= fun vol_trees ->
          return (FS.trees_to_unix_paths vol_trees) >>= function
          | [ csv ] ->
            return FS.(entry_unix_path vol.volume_entry, csv)
          | _ -> error (`sample_sheet_should_a_lonely_file sample_sheet))
        >>= fun (vol_path, csv_path) -> 
        let fc_link = 
          Template.a_link Services.flowcell [pcdata flowcell_name] (flowcell_name) in
        return [
          `sortable (flowcell_name, [fc_link]);
          `sortable (Int32.to_string mismatch, [pcdataf "%ld" mismatch]);
          `sortable (version, [pcdataf "%s" version]);
          (let tiles = (Option.value ~default:"" tiles) in
          `sortable (tiles, [codef "%s"tiles]));
          (let kind = (match kind with
            | `all_barcodes -> "All barcodes"
            | `specific_barcodes -> "Specific barcodes") in
          `sortable (kind,
                     [pcdata kind;
                      small [
                        pcdata " (";
                        a ~a:[ 
                          a_hreff "file://%s/%s/%s"
                            (Option.value ~default:"$HSROOT" 
                               (Configuration.vol_path configuration))
                            vol_path csv_path] [pcdata "file"];
                        pcdata ")"
                      ];
                     ]));
          ])
      >>= fun rows ->
      return Template.(
        let tab = 
          content_table (
            [`head [pcdata "Flowcell"]; 
             `head [pcdata "Mismatch"];
             `head [pcdata "Version"];
             `head [pcdata "Tiles Option"];
             `head [pcdata "Kind of sample-sheet"];
            ] :: rows)
        in
        content_section (pcdataf "%s: %d" name (List.length b2fs)) tab))
    else
      return (Template.content_paragraph [])

  let evaluations configuration =
    let open Html5 in
    Hitscore_lwt.db_connect configuration
    >>= fun dbh ->
    (* Bcl_to_fastqs *)
    Layout.Function_bcl_to_fastq.get_all_inserted ~dbh
    >>= bcl_to_fastq_section ~dbh ~configuration "Inserted"
    >>= fun inserted_b2fs ->
    Layout.Function_bcl_to_fastq.get_all_started ~dbh
    >>= bcl_to_fastq_section ~dbh ~configuration "Started"
    >>= fun started_b2fs ->
    Layout.Function_bcl_to_fastq.get_all_failed ~dbh
    >>= bcl_to_fastq_section ~dbh ~configuration "Failed"
    >>= fun failed_b2fs ->
    Layout.Function_bcl_to_fastq.get_all_succeeded ~dbh
    >>= bcl_to_fastq_section ~dbh ~configuration "Succeeded"
    >>= fun succeeded_b2fs ->

    return Template.(
      content_list [
        inserted_b2fs; started_b2fs; failed_b2fs; succeeded_b2fs;
      ])
    >>= fun b2f_content ->
    Hitscore_lwt.db_disconnect configuration dbh
    >>= fun _ ->
    return Template.(
      content_list [
        content_section (pcdataf "Bcl_to_fastq") b2f_content;
      ])

  let make ~configuration =
    (fun () () ->
      let main_title = "Function Evaluations" in
      Template.default ~title:main_title
        (Authentication.authorizes (`view `all_evaluations)
         >>= function
         | true ->
           Template.make_content ~configuration ~main_title    
             (evaluations configuration);
         | false ->
           Template.make_authentication_error ~configuration ~main_title
             (return [Html5.pcdataf 
                         "You may not view the function evaluations."])))


end


module Hiseq_runs_service = struct
    
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
            let vals = List.find_all lanes_info ~f:(fun a -> fst a = k) in
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
                         @ (List.flatten info_on_lanes))))
      
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

  let make configuration =
    (fun () () ->
      Template.default ~title:"HiSeq 2000 Runs"
        (Authentication.authorizes (`view `all_flowcells)
         >>= function
         | true ->
           Template.make_content ~configuration
             ~main_title:"HiSeq 2000 Runs" (hiseq_runs configuration)
         | false ->
           Template.make_authentication_error ~configuration
             ~main_title:"HiSeq 2000 Runs" 
             (return [Html5.pcdataf "You may not view all the flowcells."])))
end

module Default_service = struct
  let make hsc =
    (fun () () ->
      let open Html5 in
      let content = 
        Template.menu_ul () 
        >>= fun ul_menu ->
        let header = [h1 [pcdata "Gencore Home"];] in
        let menu =
          Option.value_map ul_menu ~default:[]
            ~f:(fun ul -> [h2 [pcdata "Menu"]; ul]) in
        let welcome = [
          h2 [pcdata "Welcome"];
          p [
            pcdata "This is Gencore's website; see also ";
            a ~a:[
              a_hreff "https://docs.google.com/a/nyu.edu/?tab=co#folders/\
                0B6RMw3n537F2OTc3ZjZlMzktZTY2YS00MmI4LTk0MmQtZmZlYzQ3Nzk3YTRl"]
              [pcdata "GenCore FAQs and Presentations"];
            pcdata " on Google-Docs.";
          ];
        ] in
        return (header @ welcome @ menu)
      in
      Template.default ~title:"Home" content)

end

let () =
  Eliom_services.register_eliom_module
    "hitscoreweb" 
    (fun () ->
      
      Lwt_preemptive.init 1 500 (eprintf "LwtP:%s\n%!");

      let _ =
        (* From the doc: http://ocsigen.org/eliom/api/server/Eliom_output
           >   Note that you should not catch every exception here
           >   since some Eliom mechanisms are done using exceptions,
           >   like redirections. Do not catch exception defined in
           >   Eliom except Eliom_common.Eliom_404,
           >   Eliom_common.Eliom_Wrong_parameter
           >   Eliom_common.Eliom_Typing_Error.

           I don't understand why we see Eliom_404 exceptions
           everywhere, and only the `real' ones get redirected to the
           404. anyway, It Works™.
        *)        
        let send ?code e =
          Lwt.bind (Template.default (error e)) (Eliom_output.Html5.send ?code)
        in
        Eliom_output.set_exn_handler (function
          | Eliom_common.Eliom_404 -> send ~code:404 `eliom_404
          | Eliom_common.Eliom_Wrong_parameter -> send `eliom_wrong_parameter
          | Eliom_common.Eliom_Typing_Error l -> send (`eliom_typing_error l)
          | Authentication.Authentication_error e -> send e
          | Layout_service.Edition_error (`layout_edit_coservice_error e) ->
            send (`layout_edit_coservice_error e)
          | e -> eprintf "EXN: %s\n%!" (Exn.to_string e); Lwt.fail e)
      in

      let hitscore_configuration =

        let pghost = ref None in
        let pgport = ref None in
        let pgdb = ref None in
        let pguser = ref None in
        let pgpass = ref None in
        let rodi = ref None in
        let vols = ref None in
        let raw = ref None in
        let hsd = ref None in
        let pam_service = ref None in
        let debug_mode = ref false in
        let open Simplexmlparser in
        let rec go_through = function
          | Element ("pghost", [], [PCData h]) -> pghost := Some h
          | Element ("pgport", [], [PCData p]) -> pgport := Some (int_of_string p)
          | Element ("pgdb", [], [PCData d]) -> pgdb := Some d
          | Element ("pguser", [], [PCData u]) -> pguser := Some u
          | Element ("pgpass", [], [PCData p]) -> pgpass := Some p
          | Element ("root-path", [], [PCData p]) -> rodi := Some p
          | Element ("vol-directory", [], [PCData p]) -> vols := Some p
          | Element ("raw-path", [], [PCData p]) -> raw := Some p
          | Element ("hiseq-dir", [], [PCData p]) -> hsd := Some p
          | Element ("debug", [], []) ->
            debug_mode := true;
          | Element ("pam-authentication-service", [], [PCData p]) ->
            pam_service := Some p;
          | Element (tag, atts, inside) ->
            Ocsigen_messages.console (fun () ->
              sprintf "Unknown Config XML Tag: %s\n" tag);
            List.iter inside go_through
          | PCData s -> ()
        in
        List.iter (Eliom_config.get_config ()) go_through;
        let db_configuration =
          match !pghost, !pgport, !pgdb, !pguser, !pgpass with
          | Some host, Some port, Some database, Some username, Some password ->
            Some (Hitscore_lwt.Configuration.db_configuration 
                    ~host ~port ~database ~username ~password)
          | _ -> None
        in
        let config =
          Hitscore_lwt.Configuration.configure
            ?vol_directory:!vols ?raw_data_path:!raw ?hiseq_directory:!hsd
            ?root_path:!rodi ?db_configuration () in
        Authentication.init ~disabled:!debug_mode ?pam_service:!pam_service config;
        config
      in

      Services.(register default) (Default_service.make hitscore_configuration);

      Services.(register home) (Default_service.make hitscore_configuration);
      
      Services.(register hiseq_runs)
        Hiseq_runs_service.(make hitscore_configuration);
      
      Services.(register persons) Persons_service.(make hitscore_configuration);
      
      Services.(register libraries) 
        Libraries_service.(make hitscore_configuration);

      Services.(register flowcell)
        Flowcell_service.(make hitscore_configuration);

      Services.(register evaluations) 
        Evaluations_service.(make ~configuration:hitscore_configuration);

      Services.(register layout) 
        Layout_service.(make ~configuration:hitscore_configuration);

      Services.(register_css stylesheet)
        Template.(css_service_handler ~configuration:hitscore_configuration);

    )

