
open Hitscoreweb_std
open Hitscore

module Msg = Hitscoreweb_messages
  
module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

module Persons_service = Hitscoreweb_persons


let qualified_name po n =
  sprintf "%s%s" Option.(value_map ~default:"" ~f:(sprintf "%s.") po) n

let qualified_link ~showing po n =
  let qn = qualified_name po n in
  [Template.a_link Services.libraries [Html5.pcdata n] (showing, [qn])]

let filter_classy_information  
    ~qualified_names ~configuration ~showing info =
  let people_filter = fun people ->
    Authentication.authorizes (`view (`libraries_of people)) in
  Data_access.filter_classy_libraries_information  
    ~qualified_names ~configuration ~people_filter info
  >>= fun filtered ->
  Authentication.authorizes (`view (`libraries_detailed_fastq_information))
  >>= fun can_view_fastq_details ->
  let filtered_time = Time.now () in
  return (object
    method static_info = info
    method filtered_on = filtered_time
    method showing = match showing with [] -> [`basic] | l -> l
    method configuration = configuration
    method qualified_names = qualified_names
    method libraries = filtered
    method can_view_fastq_details = can_view_fastq_details
  end)
    
let init_classy_information ~allowed_age ~maximal_age ~configuration =
  let info_mem = ref None in
  let condition = Lwt_condition.create () in
  let should_reconnect = ref false in
  let rec update ~configuration ~layout_cache =
    let m =
      let starting_time = Time.now () in
      if !should_reconnect
      then begin
        Backend.reconnect layout_cache#dbh
        >>= fun _ ->
        logf "Libraries classy info: reconnected."
        >>= fun () ->
        should_reconnect := false;
        return ()
      end
      else return ()
      >>= fun _ ->
      Data_access.make_classy_libraries_information ~configuration ~layout_cache
      >>= fun info ->
      info_mem := Some info;
      Lwt_condition.broadcast condition info;
      return ()
      >>= fun () ->
      logf  "{libraries} Classy info updated:\n\
               \          %s --> %s\n\
               \          %g secs\n%!"
        Time.(starting_time |! to_string)
        Time.(now () |! to_string)
        Time.(to_float (now ()) -. to_float starting_time);
    in
    double_bind m
      ~ok:return
      ~error:(fun e ->
        wrap_io Lwt_io.eprintf "Updating the classy info gave an error; \
                                reconnecting …\n"
        >>= fun () ->
        should_reconnect := true;
        logf "Libraries classy info gave an error: %s"
          (match e with
          | `Layout (_, e) -> Template.string_of_layout_error e
          | `db_backend_error _ as e -> Template.string_of_layout_error e
          | `io_exn e -> sprintf "I/O: %s" (Exn.to_string e)
          | `auth_state_exn e -> sprintf "Auth: %s" (Exn.to_string e)
          | `root_directory_not_configured ->
            sprintf "root_directory_not_configured"
          | _ -> "UNKNOWN")
        >>= fun () ->
        return ())
    >>= fun () ->
    wrap_io Lwt_unix.sleep 5.
    >>= fun () ->
    update ~configuration ~layout_cache in
  Lwt.ignore_result (
    db_connect configuration >>= fun dbh ->
    let layout_cache =
      Classy.make_cache ~allowed_age ~maximal_age ~dbh in
    update ~configuration ~layout_cache
  );
  begin fun ~qualified_names ~showing ->
    begin match !info_mem with
    | None ->
      wrap_io Lwt_condition.wait condition
      >>= fun info ->
      return info
    | Some info ->
      return info
    end
    >>= fun info ->
    filter_classy_information
      ~qualified_names ~configuration ~showing info
  end

      
let layout_id_link type_name id =
  let open Html5 in
  Template.a_link Services.layout [codef "%d" id] ([type_name], [id])
      
let intro_paragraph info =
  let open Html5 in
  let make_link (shwg, name) =
    Template.a_link Services.libraries [pcdata name]
      (shwg, info#qualified_names) in
  let links =
    List.map [ [`basic; `stock], "Metadata";
               [`basic; `fastq], "Sequencing Info";
               [`basic;`stock; `fastq], "Full" ] make_link
    |! interleave_list ~sep:(pcdata ", ") in
  [b [pcdata "Choose view: "]] @ links @ [pcdata "."] @ [br ()]

let get_fastq_stats lib sub dmux =
  let open Option in
  dmux#unaligned
  >>= fun u ->
  u#dmux_summary
  >>= fun ds ->
  let module Bui = Hitscore_interfaces.B2F_unaligned_information in
  List.find ds.(sub#lane_index - 1) (fun x ->
    x.Bui.name = lib#stock#name)

let html_detailed_dmux dmux =
  let open Template in
  let open Html5 in [
    span ~a:[
      ksprintf a_style "font-weight: bold; color: %s"
        (match dmux#b2f#g_status with
        | `Started | `Inserted -> "blue"
        | `Succeeded -> "green" | `Failed -> "red")]
      [pcdata "B2F "];
    layout_id_link "bcl_to_fastq" dmux#b2f#g_id;
    Option.(
      let default = "" in
      pcdataf " %s (mismatch: %d) %s (%s)"
        (value_map dmux#b2f#tiles ~default ~f:(sprintf "(tiles:%s)"))
        dmux#b2f#mismatch
        (value_map dmux#b2f#bases_mask
           ~default ~f:(sprintf "(bmask:%s)"))
        (value ~default:"unknown-sample-sheet-kind"
           (dmux#assembly >>= fun a ->
            return a#kind
            >>| Layout.Enumeration_sample_sheet_kind.to_string)))
  ]
                 
let html_detailed_deliveries ?(append_path=false) subm dmux =
  let open Template in
  let open Html5 in
  let actual_deliveries =
    List.filter_map dmux#deliveries (fun del ->
      let open Option in
      List.find subm#invoices (fun i -> i#g_id = del#oo#invoice#id)
      >>= fun inv ->
      Some (del, inv)) in
  begin match actual_deliveries with
  | [] -> [pcdata "Never delivered"]
  | l ->
    (interleave_map l ~sep:(br ())
       ~f:(fun (del, inv) ->
         let path = 
           Option.(value_map ~default:"NO-DIR"
                     ~f:(fun d-> d#directory) del#client_fastqs_dir) in
         let path_part =
           if append_path then
             span [strong [pcdata ", Path: "]; codef "%s" path]
           else
             span [] in
         span ~a:[a_title path ]
           [strong [pcdata "Delivery "];
            layout_id_link "prepare_unaligned_delivery" del#oo#g_id;
            strong [pcdata "; Invoice "]; 
            layout_id_link "invoicing" inv#g_id;
            path_part
           ]))
  end
    
let detailed_fastq_subtable lib =
  let open Template in
  let open Html5 in
  let module Bui = Hitscore_interfaces.B2F_unaligned_information in
  let subtable =
    List.map lib#submissions (fun sub ->
      List.map sub#flowcell#hiseq_raws (fun hs ->
        List.map hs#demultiplexings (fun dmux ->
          let fastq_stats = get_fastq_stats lib sub dmux in
          let fcid = sub#flowcell#oo#serial_name in
          [ `sortable (sprintf "%s:%d" fcid sub#lane_index,
                       [a_link Services.flowcell [pcdata fcid] fcid;
                        pcdataf " (Lane %d)"  sub#lane_index; ]);
            `text (html_detailed_dmux dmux);
            `text (html_detailed_deliveries sub dmux);
            `text Option.(
              value_map fastq_stats ~default:[pcdata "—"]
                ~f:(fun s ->
                  [codef "%s"
                      (pretty_string_of_float ~sof:(sprintf "%.0f")
                         s.Bui.cluster_count)]));
            `text Option.(
              value_map fastq_stats ~default:[pcdata "—"]
                ~f:(fun s ->
                  [codef "%.2f%%" (100. *. s.Bui.yield_q30 /. s.Bui.yield) ]));
            `text Option.(
              value_map fastq_stats ~default:[pcdata "—"]
                ~f:(fun s ->
                  [codef "%.2f" (s.Bui.quality_score_sum /. s.Bui.yield)]));
            
        ])) |! List.concat) |! List.concat in
  let empty_row = [List.init 6 (fun _ -> `text [pcdata ""])] in
  `subtable (if List.concat subtable = [] then empty_row else subtable)

let choose_delivery_for_user dmux sub =
  let open Option in
  List.filter_map dmux#deliveries (fun del ->
    if del#oo#g_status <> `Succeeded
    then None
    else (
      List.find sub#invoices (fun i -> i#g_id = del#oo#invoice#id)
                                       >>= fun inv ->
      Some (del, inv)))
  |! List.reduce ~f:(fun (d1, i1) (d2, i2) ->
    if d1#oo#g_completed < d2#oo#g_completed
    then (d2, i2)
    else (d1, i1))
      
let simple_fastq_subtable lib =
  let open Template in
  let open Html5 in
  let module Bui = Hitscore_interfaces.B2F_unaligned_information in
  let subtable =
    List.map lib#submissions (fun sub ->
      List.map sub#flowcell#hiseq_raws (fun hs ->
        List.filter_map hs#demultiplexings (fun dmux ->
          let open Option in
          let fastq_stats = get_fastq_stats lib sub dmux in
          let fcid = sub#flowcell#oo#serial_name in
          choose_delivery_for_user dmux sub
          >>= fun (del, inv) ->
          return [
            `sortable (sprintf "%s:%d" fcid sub#lane_index,
                       [a_link Services.flowcell [pcdata fcid] fcid;
                        pcdataf " (Lane %d)"  sub#lane_index; ]);
            `text [
              Option.(
                let default = "" in
                pcdataf " %s (mismatch: %d) %s (%s)"
                  (value_map dmux#b2f#tiles ~default ~f:(sprintf "(tiles:%s)"))
                  dmux#b2f#mismatch
                  (value_map dmux#b2f#bases_mask
                     ~default ~f:(sprintf "(bmask:%s)"))
                  (value ~default:"unknown-sample-sheet-kind"
                     (dmux#assembly >>= fun a ->
                      return a#kind
                      >>| Layout.Enumeration_sample_sheet_kind.to_string)));
            ];
            `text Option.(
              value_map fastq_stats ~default:[pcdata "—"]
                ~f:(fun s ->
                  [codef "%s"
                      (pretty_string_of_float ~sof:(sprintf "%.0f")
                         s.Bui.cluster_count)]));
            `text Option.(
              value_map fastq_stats ~default:[pcdata "—"]
                ~f:(fun s ->
                  [codef "%.2f%%" (100. *. s.Bui.yield_q30 /. s.Bui.yield) ]));
            `text Option.(
              value_map fastq_stats ~default:[pcdata "—"]
                ~f:(fun s ->
                  [codef "%.2f" (s.Bui.quality_score_sum /. s.Bui.yield)]));
          ]) |! List.concat) |! List.concat) in
  let empty_row = [List.init 5 (fun _ -> `text [pcdata ""])] in
  `subtable (if List.concat subtable = [] then empty_row else subtable)

let make_file_links vol_id paths =
  let open Html5 in
  List.map paths begin fun path ->
    if Eliom_registration.File.check_file path
    then begin match Filename.split_extension path |! snd with
    | Some ext ->
      b [Template.a_link Services.file [pcdata ext] (vol_id, path)]
    | None ->
      b [Template.a_link Services.file [pcdata "???"] (vol_id, path)]
    end
    else 
      i ~a:[ a_title path ] [pcdata "missing-file"]
  end 
  |! interleave_list ~sep:(pcdata ", ")

let libraries_table info =
  let open Template in
  let open Html5 in
  let everywhere f = ([`basic; `stock; `fastq], f) in
  let basic f = ([`basic], f) in
  let stock f = ([`stock], f) in
  let fastq f = ([`fastq], f) in
  let rows =
    List.map info#libraries (fun lib -> [
      everywhere (fun () ->
        `sortable (lib#stock#name,
                   qualified_link ~showing:info#showing
                     lib#stock#project lib#stock#name));
      everywhere (fun () -> cell_option lib#stock#project);
      basic (fun () -> cell_option lib#stock#description);
      basic (fun () -> cell_option (Option.map lib#sample (fun s -> s#sample#name)));
      basic (fun () ->
        cell_option Option.(lib#sample >>= fun s -> s#organism >>= fun o -> o#name));
      stock (fun () ->
        `sortable (
          List.length lib#submissions |! Int.to_string,
          begin match lib#submissions with
          | [] -> [pcdata "Never"]
          | l ->
            List.map l (fun sub ->
              let fcid = sub#flowcell#oo#serial_name in
              span [
                a_link Services.flowcell [pcdata fcid] fcid;
                pcdataf " (Lane %d)"  sub#lane_index;
                br ();
              ])
          end));
      basic (fun () ->
        cell_fmt "%s" (String.concat ~sep:"/" (Array.to_list lib#stock#application)));
      stock (fun () ->
        let f b =
          match b#kind with
          | `custom ->
            sprintf "Custom[%s%s%s%s]" (Option.value ~default:"None" b#sequence)
              Option.(value_map b#position_in_r1 ~default:"" ~f:(sprintf "-R1:%d"))
              Option.(value_map b#position_in_r2 ~default:"" ~f:(sprintf "-R2:%d"))
              Option.(value_map b#position_in_index ~default:"" ~f:(sprintf "-I:%d"))
          | `bioo     -> sprintf     "BIOO[%d]" (Option.value ~default:0 b#index)
          | `bioo_96  -> sprintf  "BIOO-96[%d]" (Option.value ~default:0 b#index) 
          | `illumina -> sprintf "Illumina[%d]" (Option.value ~default:0 b#index)
          | `nugen    -> sprintf    "NuGen[%d]" (Option.value ~default:0 b#index)
        in
        cell_text (String.concat ~sep:" OR "
                     (List.map lib#barcoding (fun l ->
                       String.concat ~sep:" AND " (List.map l ~f)))));
      stock (fun () -> cell_int_option lib#stock#p5_adapter_length);
      stock (fun () -> cell_int_option lib#stock#p7_adapter_length);
      stock (fun () -> cell_text (Bool.to_string lib#stock#stranded));
      stock (fun () -> cell_text (Bool.to_string lib#stock#truseq_control));
      stock (fun () -> cell_option lib#stock#rnaseq_control);
      stock (fun () ->
        Option.value_map lib#preparator ~default:(`text []) ~f:(fun p ->
          `sortable (p#email,
                     [a_link Services.persons [pcdata p#email] (None, [p#email])])));
      stock (fun () ->
        let subtable =
          List.map lib#bioanalyzers (fun b ->
            [ cell_int_option b#bioanalyzer#well_number;
              cell_fmt_option "%.2f" b#bioanalyzer#mean_fragment_size;
              cell_fmt_option "%.2f" b#bioanalyzer#min_fragment_size;
              cell_fmt_option "%.2f" b#bioanalyzer#max_fragment_size;
              `text (Option.value_map b#bioanalyzer#files
                       ~f:(fun v -> make_file_links v#id b#paths) ~default:[]);
            ]) in
        if lib#bioanalyzers = []
        then `subtable [List.init 5 (fun _ -> cell_text "")]
        else `subtable subtable);
      stock (fun () ->
        let subtable =
          List.map lib#agarose_gels (fun a ->
            [ cell_int_option a#agarose_gel#well_number;
              cell_fmt_option "%.2f" a#agarose_gel#mean_fragment_size;
              cell_fmt_option "%.2f" a#agarose_gel#min_fragment_size;
              cell_fmt_option "%.2f" a#agarose_gel#max_fragment_size;
              `text (Option.value_map a#agarose_gel#files
                       ~f:(fun v -> make_file_links v#id a#paths) ~default:[]);
            ]) in
        if lib#agarose_gels = []
        then `subtable [List.init 5 (fun _ -> cell_text "")]
        else `subtable subtable);
      stock (fun () ->
        Option.value_map lib#protocol ~default:(`text [])
          ~f:(fun p ->
            match lib#protocol_paths with
            | None | Some [] -> cell_text (sprintf "%s (no file)" p#name)
            | Some paths ->
              `sortable (p#name,
                         List.concat [
                           [pcdataf "%s (" p#name];
                           make_file_links p#doc#id  paths;
                           [pcdata ")"]
                         ])
          ));
      stock (fun () -> cell_option lib#stock#note) ;
              
      fastq (fun () ->
        if info#can_view_fastq_details then
          detailed_fastq_subtable lib
        else
          simple_fastq_subtable lib)
      ]) in
  let first_row =
    let fastq_part =
      if info#can_view_fastq_details then
        [fastq (fun () -> `head [pcdata "Demultiplexing"]);
         fastq (fun () -> `head [pcdata "Delivery"]);
         fastq (fun () -> `head_cell Msg.number_of_reads);
         fastq (fun () -> `head_cell Msg.percent_bases_over_q30);
         fastq (fun () -> `head_cell Msg.mean_qs); ]
      else
        [fastq (fun () -> `head_cell Msg.demux_options);
         fastq (fun () -> `head_cell Msg.number_of_reads);
         fastq (fun () -> `head_cell Msg.percent_bases_over_q30);
         fastq (fun () -> `head_cell Msg.mean_qs); ]
    in
    [
      everywhere (fun () -> `head_cell Msg.library_name);
      everywhere (fun () -> `head_cell Msg.library_project);
      basic (fun () -> `head_cell Msg.library_description);
      basic (fun () -> `head_cell Msg.sample_name);
      basic (fun () -> `head_cell Msg.organism_name);
      stock (fun () -> `head_cell Msg.library_submissions);
      basic (fun () -> `head_cell Msg.library_application);
      stock (fun () -> `head_cell Msg.library_barcode);
      stock (fun () -> `head_cell Msg.library_p5);
      stock (fun () -> `head_cell Msg.library_p7);
      stock (fun () -> `head_cell Msg.library_stranded);
      stock (fun () -> `head_cell Msg.library_truseq_control);
      stock (fun () -> `head_cell Msg.library_rnaseq_control);
      stock (fun () -> `head_cell Msg.library_preparator);

      stock (fun () -> `head_cell Msg.bioanalyzer_well_nb);
      stock (fun () -> `head_cell Msg.bioanalyzer_mean);
      stock (fun () -> `head_cell Msg.bioanalyzer_min);
      stock (fun () -> `head_cell Msg.bioanalyzer_max);
      stock (fun () -> `head_cell Msg.bioanalyzer_files);
      stock (fun () -> `head_cell Msg.agarose_gel_well_nb);
      stock (fun () -> `head_cell Msg.agarose_gel_mean);
      stock (fun () -> `head_cell Msg.agarose_gel_min);
      stock (fun () -> `head_cell Msg.agarose_gel_max);
      stock (fun () -> `head_cell Msg.agarose_gel_files);
      stock (fun () -> `head_cell Msg.protocol);
      stock (fun () -> `head_cell Msg.library_note);
      fastq (fun () -> `head_cell Msg.library_submissions);
    ] @ fastq_part
  in
  let table =
    List.map (first_row :: rows) ~f:(fun row ->
      List.filter_map row (fun (where, what) ->
        if List.exists where ~f:(fun w -> List.exists info#showing ((=) w))
        then Some (what ()) else None)) in
  
  Template.content_table ~style:`alternate_colors table
    
let benchmarks work_started info_got table_generated info =
  let open Html5 in
  Authentication.authorizes (`view `benchmarks) >>= fun can_view ->
  if can_view then (
    let row title time =
      [ `head [pcdata title];
        `text [pcdata Time.(to_string time)];
        `text [pcdataf "%f" Time.(to_float time -. to_float work_started)]
      ] in
    return Template.(content_section (pcdata "Benchmarks")
                       (content_table [
                         row "work_started" work_started;
                         row "static info started"
                           info#static_info#creation_started_on;
                         row "static info created" info#static_info#created_on;
                         row "info filtered" info#filtered_on;
                         row "info_got" info_got;
                         row "table_generated" table_generated;
                       ]))
  ) else
    return Template.(content_list [])
      
let fastq_path unaligned_path lane_index lib_name barcode read =
  sprintf "%s/Unaligned/Project_Lane%d/Sample_%s/%s_%s_L00%d_R%d_001.fastq.gz"
    unaligned_path lane_index lib_name lib_name barcode
    lane_index read
let fastxqs_path fastxqs_path lane_index lib_name barcode read =
  sprintf "%s/Unaligned/Project_Lane%d/Sample_%s/%s_%s_L00%d_R%d_001.fxqs"
    fastxqs_path lane_index lib_name lib_name barcode
    lane_index read
    
let fastx_table path =
  let open Html5 in
  let open Template in
  Data_access_types.(
    Data_access.File_cache.get_fastx_quality_stats path
    >>| List.map ~f:(fun {
      bfxqs_column; bfxqs_count; bfxqs_min; bfxqs_max;
      bfxqs_sum; bfxqs_mean; bfxqs_Q1; bfxqs_med;
      bfxqs_Q3; bfxqs_IQR; bfxqs_lW; bfxqs_rW;
      bfxqs_A_Count; bfxqs_C_Count; bfxqs_G_Count; bfxqs_T_Count; bfxqs_N_Count;
      bfxqs_Max_count;} ->
      let nb0 f = `number (sprintf "%.0f", f) in 
      let nb2 f = `number (sprintf "%.2f", f) in 
      [nb0 bfxqs_column; nb0 bfxqs_count; nb0 bfxqs_min; nb0 bfxqs_max;
       nb0 bfxqs_sum; nb2 bfxqs_mean; nb0 bfxqs_Q1; nb0 bfxqs_med;
       nb0 bfxqs_Q3; nb0 bfxqs_IQR; nb0 bfxqs_lW; nb0 bfxqs_rW;
       nb0 bfxqs_A_Count; nb0 bfxqs_C_Count; nb0 bfxqs_G_Count;
       nb0 bfxqs_T_Count; nb0 bfxqs_N_Count;
       nb0 bfxqs_Max_count;]))
  >>= fun rows ->
  let h s = `head [pcdata s] in
  return (  
    [h "column"; h "count"; h "min"; h "max";
     h "sum"; h "mean"; h "Q1"; h "med";
     h "Q3"; h "IQR"; h "lW"; h "rW";
     h "A_Count"; h "C_Count"; h "G_Count"; h "T_Count"; h "N_Count";
     h "Max_count";]
    :: rows)
    
let rendered_fastx_table path =
  let open Html5 in
  let open Template in
  double_bind (fastx_table path)
    ~ok:(fun o ->
      let msg, div =
        (Template.hide_show_div
           ~show_message:"Show FASTX quality stats table"
           ~hide_message:"Hide FASTX quality stats table"
           [Template.html_of_content (content_table o)]) in
      return [Template.pretty_box [msg; div]])
    ~error:(fun e ->
      let errf fmt =
        ksprintf (fun s -> [br(); Template.error_span [pcdata s]]) fmt in
      begin match e with
      | `empty_fastx_quality_stats s ->
        return (errf "ERROR: file %s gave empty quality stats" s)
      | `error_in_fastx_quality_stats_parsing (s, sll) ->
        return (errf "ERROR: parsing file %s gave an error (%d)"
                  s (List.length sll))
      | `read_file_error (f, e) ->
        return (errf "I/O Error in with fastx (file %S): %s\n%!" f (Exn.to_string e))
      end)

let fastx_quality_plots path =
  let open Html5 in
  let open Template in
  let make_chart =
    let open Data_access_types in
    Data_access.File_cache.get_fastx_quality_stats path
    >>= fun stats ->
    while_sequential stats ~f:(fun {
      bfxqs_column; bfxqs_count; bfxqs_min; bfxqs_max;
      bfxqs_sum; bfxqs_mean; bfxqs_Q1; bfxqs_med;
      bfxqs_Q3; bfxqs_IQR; bfxqs_lW; bfxqs_rW;
      bfxqs_A_Count; bfxqs_C_Count; bfxqs_G_Count; bfxqs_T_Count; bfxqs_N_Count;
      bfxqs_Max_count;} ->
      return (
        [ "A", bfxqs_A_Count;
          "C", bfxqs_C_Count;
          "G", bfxqs_G_Count;
          "T", bfxqs_T_Count;
          "N", bfxqs_N_Count;],
        (bfxqs_lW, bfxqs_Q1, bfxqs_med, bfxqs_Q3, bfxqs_rW)))
    >>| List.unzip
    >>= fun (acgtn, by5) ->
    Highchart.make ~more_y:5. ~y_axis_title:"Q Score"
      ~plot_title:"Quality Plot" [`box_whisker by5]
    >>= fun qplot ->
    let msg, div =
      (Template.hide_show_div ~start_hidden:false
         ~show_message:"Show Q Score Box-Whisker plot"
         ~hide_message:"Hide Q Score Box-Whisker plot"
         qplot) in
    return [Template.pretty_box [msg; div]]
    >>= fun qplot ->
    Highchart.make ~more_y:5. ~y_axis_title:"Counts" ~with_legend:true
      ~plot_title:"ACGTN Distribution" [`stack acgtn]
    >>= fun acgtnplot ->
    let msg, div =
      (Template.hide_show_div
         ~show_message:"Show ACGTN distribution plot"
         ~hide_message:"Hide ACGTN distribution plot"
         acgtnplot) in
    return [Template.pretty_box [msg; div]]
    >>= fun acgtnplot ->
    return (acgtnplot @ qplot)
  in
  let errf fmt =
    ksprintf (fun s -> [br(); Template.error_span [pcdata s]]) fmt in
  double_bind make_chart 
    ~ok:(fun chart ->
      return chart)
    ~error:(fun _ -> return (errf "ERROR while getting the fastx plot"))

let demuxable_barcode_sequences lane lib =
  if List.length lane#inputs = 1 then
    ["NoIndex"]
  else
    match lib#barcoding with
    | [and_list] ->
      if List.for_all and_list (fun o ->
        (o#kind = `illumina || o#kind = `bioo) && o#index <> None)
      then
        List.filter_map and_list (fun o ->
          match o#kind with
          | `illumina ->
            List.Assoc.find
              Assemble_sample_sheet.illumina_barcodes (Option.value_exn o#index)
          | `bioo ->
            List.Assoc.find
              Assemble_sample_sheet.bioo_barcodes (Option.value_exn o#index)
          | _ -> None)
      else []
    | _ -> []
        
let per_lirbary_details info =
  let open Html5 in
  let open Template in
  while_sequential info#libraries (fun lib ->
    while_sequential lib#submissions (fun sub ->
      while_sequential sub#flowcell#hiseq_raws (fun hr ->
        let fcid = sub#flowcell#oo#serial_name in
        let section_title = 
          span [pcdata "Submission ";
                a_link Services.flowcell [pcdata fcid] fcid;
                pcdataf " (Lane %d)"  sub#lane_index;] in
        while_sequential hr#demultiplexings (fun dmux ->
          let section_title =
            span (pcdata "Demultiplexing " :: html_detailed_dmux dmux) in
          let deliv_par = [
            pcdata "Delivery status: ";
            ul [
              li (html_detailed_deliveries ~append_path:true sub dmux);
            ];
          ] in
          map_option dmux#unaligned (fun un ->
            let barcodes = demuxable_barcode_sequences sub#lane lib in
            let fastq_r1s =
              List.map barcodes (fun seq ->
                fastq_path un#path sub#lane_index lib#stock#name seq 1) in
            let fastq_r2s =
              Option.value_map ~default:[]
                sub#lane#oo#requested_read_length_2 ~f:(fun _ ->
                  List.map barcodes (fun seq ->
                    fastq_path un#path sub#lane_index lib#stock#name seq 2))
            in
            while_sequential un#fastx_paths (fun path ->
              
              let origins =
                interleave_map ~sep:[br ()] un#fastx_qss ~f:(fun fxqs ->
                  [pcdata "Function ";
                   layout_id_link "fastx_quality_stats" fxqs#g_id; ])
                |! List.concat
              in
              let rorigins =
                interleave_map ~sep:[br ()] un#fastx_results ~f:(fun fxqsr ->
                  [pcdata "Record ";
                   layout_id_link "fastx_quality_stats_result" fxqsr#g_id; ])
                |! List.concat
              in
              let fastxqs_r1s =
                List.map barcodes (fun seq ->
                  ("R1", fastxqs_path path sub#lane_index lib#stock#name seq 1)) in
              let fastxqs_r2s =
                Option.value_map ~default:[]
                  sub#lane#oo#requested_read_length_2 ~f:(fun _ ->
                    List.map barcodes (fun seq ->
                      ("R2", fastxqs_path path sub#lane_index lib#stock#name seq 2)))
              in
              while_sequential (fastxqs_r1s @ fastxqs_r2s) (fun (kind, path) ->
                rendered_fastx_table path >>= fun fastx_table ->
                fastx_quality_plots path >>= fun fastx_qplot ->

                return [
                  strongf "%s: " kind;
                  codef "%s" path;
                  div fastx_table;
                  div fastx_qplot;
                ])
              >>= fun fastx_items ->
              let details =
                div [strongf "Fastx Quality Stats:";
                     ul [li origins; li rorigins; ];
                     div [ul (List.map fastx_items li)];
                    ] in
              
              return (details))
            >>= fun fastx_stuff ->
            let unaligned_par =
              let fastqs =
                interleave_map fastq_r1s ~sep:[]
                  ~f:(fun p -> [strongf "R1: "; codef "%s" p; br ()])
                @ interleave_map fastq_r2s ~sep:[]
                  ~f:(fun p -> [strongf "R2: "; codef "%s" p; br ()])
                |! List.concat
              in
              let details =
                div [
                  pcdata "Unaligned Directory: ";
                  ul [
                    li [strong [pcdata "Value: "];
                        layout_id_link "bcl_to_fastq_unaligned" un#b2fu#g_id;
                        strong [pcdata "; Volume: "];
                        layout_id_link "bcl_to_fastq_unaligned_opaque" un#vol#g_id];
                    li [strong [pcdata "Path: "]; codef "%s" un#path];
                    li fastqs;

                  ]] in
              content_paragraph [div deliv_par; details; div fastx_stuff] in
            return [unaligned_par])
          >>| Option.value ~default:[]
          >>= fun unaligned_related_sections ->
          let content = unaligned_related_sections in
          return (content_section section_title (content_list content)))
        >>= fun dmux_sections ->
        return (content_section section_title (content_list dmux_sections))))
    >>| List.concat
    >>= fun submission_sections ->
    return (content_section (pcdataf "Library %s" lib#stock#name)
              (content_list submission_sections)))
  >>= fun details_sections ->
  return (content_list details_sections)

let per_lirbary_simple_details info =
  let open Html5 in
  let open Template in
  while_sequential info#libraries (fun lib ->
    while_sequential lib#submissions (fun sub ->
      while_sequential sub#flowcell#hiseq_raws (fun hr ->
          let fcid = sub#flowcell#oo#serial_name in
          let section_title = 
            span [pcdata "Submission ";
                  a_link Services.flowcell [pcdata fcid] fcid;
                  pcdataf " (Lane %d)"  sub#lane_index;] in
          while_sequential hr#demultiplexings (fun dmux ->
            map_option (choose_delivery_for_user dmux sub) (fun (del, inv) ->
              map_option dmux#unaligned (fun un ->
                while_sequential un#fastx_paths (fun path ->
                  let barcodes = demuxable_barcode_sequences sub#lane lib in
                  let fastxqs_r1s =
                    List.map barcodes (fun seq ->
                      ("Read 1",
                       fastxqs_path path sub#lane_index lib#stock#name seq 1)) in
                  let fastxqs_r2s =
                    Option.value_map ~default:[]
                      sub#lane#oo#requested_read_length_2 ~f:(fun _ ->
                        List.map barcodes (fun seq ->
                          ("Read 2",
                           fastxqs_path path sub#lane_index lib#stock#name seq 2)))
                  in
                  while_sequential (fastxqs_r1s @ fastxqs_r2s) (fun (kind, path) ->
                    rendered_fastx_table path >>= fun fastx_table ->
                    fastx_quality_plots path >>= fun fastx_qplot ->
                    return [
                      strongf "%s: " kind;
                      div fastx_table;
                      div fastx_qplot;
                    ]))
                >>| List.concat
                >>= fun fastx_items ->
                let details =
                  div [strongf "Fastx Quality Stats:";
                       div [ul (List.map fastx_items li)];] in
                return details)
              >>= fun stats ->
              let path = 
                Option.(value_map ~default:"NO-DIR"
                          ~f:(fun d-> d#directory) del#client_fastqs_dir) in
              return (content_paragraph [
                strongf "Delivered in ";
                codef "%s" path;
                div [Option.value ~default:(strongf "No stats available")
                        stats];
              ])))
          >>| List.filter_opt
          >>= fun delivery_sections ->
          return (content_section section_title (content_list delivery_sections))))
    >>| List.concat
    >>= fun submission_sections ->
    return (content_section (pcdataf "Library %s" lib#stock#name)
              (content_list submission_sections)))
  >>= fun details_sections ->
  return (content_list details_sections)

  

    
let libraries work_started info_got info =
  let open Html5 in

  let libraries_table = libraries_table info in
  let table_generated = Time.now () in
  benchmarks work_started info_got table_generated info >>= fun benchmarks ->
  (if List.length info#libraries = 1 || List.exists info#showing ((=) `details)
   then if info#can_view_fastq_details
     then per_lirbary_details info
     else per_lirbary_simple_details info
   else return (Template.content_list []))
  >>= fun details ->
  let libnb = List.length info#libraries in
  return Template.(
    content_section (pcdataf "Viewing %d Librar%s" libnb
                       (if libnb = 1 then "y" else "ies"))
      (content_list [
        benchmarks;
        content_section (pcdataf "Summary Table")
          (content_list [
            content_paragraph (intro_paragraph info);
            libraries_table;
           ]);
        details;
      ]))
  

let make ~information_cache_timming ~configuration =
  let allowed_age, maximal_age = information_cache_timming in
  let classy_info =
    init_classy_information ~allowed_age ~maximal_age ~configuration in
  (fun (showing, qualified_names) () ->
    let work_started = Time.now () in
    let main_title = "Libraries" in
    Template.default ~title:main_title
      (Authentication.authorizes (`view `libraries)
       >>= function
       | true ->
         Template.make_content ~configuration ~main_title (
           classy_info ~qualified_names ~showing
           >>= fun info ->
           let info_got = Time.now () in
           libraries work_started info_got info)
       | false ->
         Template.make_authentication_error ~configuration ~main_title
           (return [Html5.pcdataf "You may not view the libraries."])))
