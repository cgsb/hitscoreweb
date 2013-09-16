
open Hitscoreweb_std_server
open Hitscore

module Msg = Hitscoreweb_messages

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

module Persons_service = Hitscoreweb_persons
module Web_data_access = Hitscoreweb_data_access


let qualified_name po n =
  sprintf "%s%s" Option.(value_map ~default:"" ~f:(sprintf "%s.") po) n

let qualified_link ~showing po n =
  let qn = qualified_name po n in
  [Template.a_link Services.libraries [Html5.pcdata n] (showing, [qn])]

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
    |> interleave_list ~sep:(pcdata ", ") in
  [b [pcdata "Choose view: "]] @ links @ [pcdata "."] @ [br ()]


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
         let path, host=
           Option.value_map ~default:("NO-DIR", "NO-CLUSTER")
             ~f:(fun d-> (d#directory, d#host)) del#client_fastqs_dir in
         let path_part =
           if append_path then
             span [strong [pcdata ", Path: "];
                   codef "%s" path;
                   strong [pcdata ", Cluster: "; html_of_cluster host;]]
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
          let fastq_stats = Data_access.get_fastq_stats lib sub dmux in
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

        ])) |> List.concat) |> List.concat in
  let empty_row = [List.init 6 (fun _ -> `text [pcdata ""])] in
  `subtable (if List.concat subtable = [] then empty_row else subtable)


let simple_fastq_subtable ~classy_cache ~user_opt lib =
  let open Template in
  let open Html5 in
  let module Bui = Hitscore_interfaces.B2F_unaligned_information in
  let subtable =
    List.map lib#submissions (fun sub ->
        List.map sub#flowcell#hiseq_raws (fun hs ->
            List.filter_map hs#demultiplexings (fun dmux ->
                let open Option in
                let fastq_stats = Data_access.get_fastq_stats lib sub dmux in
                let fcid = sub#flowcell#oo#serial_name in
                let deliveries = dmux#deliveries in
                some_if (deliveries <> []) (
                  List.filter_map deliveries (fun del ->
                      let delivery_in_meta_hiseq_runs =
                        List.find_map classy_cache#meta_hiseq_runs (fun mhr ->
                            List.find mhr#deliveries (fun dd ->
                                dd#delivery#oo#g_id = del#oo#g_id))
                      in
                      delivery_in_meta_hiseq_runs
                      >>= fun deliv ->
                      let filtered_lanes =
                        List.filter deliv#lanes (fun l ->
                            let contacts = Array.to_list l#lane#contacts in
                            List.exists contacts (fun c -> Some c#pointer = user_opt))
                      in
                      some_if (filtered_lanes <> []) [
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
                        `text [pcdata "In ";
                               codef "%s" (value_map del#client_fastqs_dir
                                             ~default:"???"
                                             ~f:(fun c -> c#directory))];
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
                      ])))) |> List.concat) |> List.concat |> List.concat in
  let empty_row = [List.init 6 (fun _ -> `text [pcdata ""])] in
  `subtable (if List.concat subtable = [] then empty_row else subtable)

let make_file_links vol_id paths =
  let open Html5 in
  List.map paths begin fun path ->
    if Eliom_registration.File.check_file path
    then begin match Filename.split_extension path |> snd with
    | Some ext ->
      b [Template.a_link Services.file [pcdata ext] (vol_id, path)]
    | None ->
      b [Template.a_link Services.file [pcdata "???"] (vol_id, path)]
    end
    else
      i ~a:[ a_title path ] [pcdata "missing-file"]
  end
  |> interleave_list ~sep:(pcdata ", ")

let libraries_table ~showing ~can_view_fastq_details ~classy_cache ~user_opt info =
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
                   qualified_link ~showing lib#stock#project lib#stock#name));
      everywhere (fun () -> cell_option lib#stock#project);
      basic (fun () -> cell_option lib#stock#description);
      basic (fun () -> cell_option (Option.map lib#sample (fun s -> s#sample#name)));
      basic (fun () ->
        cell_option Option.(lib#sample >>= fun s -> s#organism >>= fun o -> o#name));
      stock (fun () ->
        `sortable (
          List.length lib#submissions |> Int.to_string,
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
          let o o default = Option.value ~default o in
          sprintf "%s:%s[%s:%s:%s]"
            (o b#provider "Custom") (o b#name "?")
            (o b#read "?")
            (Option.value_map b#position ~default:"?" ~f:(sprintf "%d"))
            (o b#sequence "?")
        in
        cell_text (String.concat ~sep:" OR "
                     (List.map lib#barcoding (fun l ->
                          String.concat ~sep:" AND " (List.map l ~f)))));
      stock (fun () -> cell_int_option lib#stock#x_adapter_length);
      stock (fun () -> cell_int_option lib#stock#y_adapter_length);
      stock (fun () -> cell_text (Bool.to_string lib#stock#stranded));
      stock (fun () ->
          cell_option (Option.map ~f:Bool.to_string lib#stock#truseq_control));
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
        if can_view_fastq_details then
          detailed_fastq_subtable lib
        else
          simple_fastq_subtable ~classy_cache ~user_opt lib)
      ]) in
  let first_row =
    let fastq_part =
      if can_view_fastq_details then
        [fastq (fun () -> `head [pcdata "Demultiplexing"]);
         fastq (fun () -> `head [pcdata "Delivery"]);
         fastq (fun () -> `head_cell Msg.number_of_reads);
         fastq (fun () -> `head_cell Msg.percent_bases_over_q30);
         fastq (fun () -> `head_cell Msg.mean_qs); ]
      else
        [fastq (fun () -> `head_cell Msg.demux_options);
         fastq (fun () -> `head [pcdata "Delivery"]);
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
        if List.exists where ~f:(fun w -> List.exists showing ((=) w))
        then Some (what ()) else None)) in
  let progressive =
    if List.length table > 300 then Some 10 else None in
  Template.content_table ~style:`alternate_colors table ?progressive

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
      return (Some [Template.pretty_box [msg; div]]))
    ~error:(fun e ->
      let errf fmt =
        ksprintf (fun s ->
          Template.error_box "No Fastx Stats" "%s" s
          >>= fun box ->
          return (Some [br(); box])) fmt in
      begin match e with
      | `empty_fastx_quality_stats s ->
        (errf "file %s gave empty quality stats" s)
      | `error_in_fastx_quality_stats_parsing (s, sll) ->
        (errf "parsing file %s gave an error (%d)" s (List.length sll))
      | `read_file_timeout (f, t) ->
        (errf "I/O Error in with fastx (file %S): timeout %f\n%!" f t)
      | `read_file_error (f, (Unix.Unix_error (_, "open", _) as e)) ->
        logf "Fastx file (%S) cannot be opened\n%s\n%!" f (Exn.to_string e)
        >>= fun () ->
        return None
      | `read_file_error (f, e) ->
        (errf "I/O Error in with fastx (file %S): %s\n%!" f (Exn.to_string e))
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
  double_bind make_chart
    ~ok:(fun chart -> return chart)
    ~error:(fun _ -> (* error already `displayed' *) return [])

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
            let files =
              Data_access.user_file_paths ~unaligned:un ~submission:sub lib in
            let fastq_r1s, fastq_r2s = files#fastq_r1s, files#fastq_r2s in
            while_sequential files#fastx (fun (r1, r2) ->
              let origins =
                interleave_map ~sep:[br ()] un#fastx_qss ~f:(fun fxqs ->
                  [pcdata "Function ";
                   layout_id_link "fastx_quality_stats" fxqs#g_id; ])
                |> List.concat
              in
              let rorigins =
                interleave_map ~sep:[br ()] un#fastx_results ~f:(fun fxqsr ->
                  [pcdata "Record ";
                   layout_id_link "fastx_quality_stats_result" fxqsr#g_id; ])
                |> List.concat
              in
              while_sequential (r1 @ r2) (fun fastx_read ->
                rendered_fastx_table fastx_read#path
                >>= begin function
                | Some fastx_table ->
                  fastx_quality_plots fastx_read#path >>= fun fastx_qplot ->
                  return [
                      strongf "%s: " fastx_read#kind;
                      codef "%s" fastx_read#path;
                      div fastx_table;
                      div fastx_qplot;
                    ]
                | None -> return []
                end)
              >>= fun fastx_items ->
              let details =
                if fastx_items = []
                then div [strongf "No details for this one."]
                else
                  let li_items =
                    List.filter_map fastx_items
                      (function [] -> None | l -> Some (li l)) in
                  div [strongf "Fastx Quality Stats:";
                       ul [li origins; li rorigins; ];
                       div [ul li_items];]
              in
              return (details))
            >>= fun fastx_stuff ->
            let unaligned_par =
              let fastqs =
                interleave_map fastq_r1s ~sep:[]
                  ~f:(fun p -> [strongf "R1: "; codef "%s" p; br ()])
                @ interleave_map fastq_r2s ~sep:[]
                  ~f:(fun p -> [strongf "R2: "; codef "%s" p; br ()])
                |> List.concat
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
            map_option (Data_access.choose_delivery_for_user dmux sub) (fun (del, inv) ->
              map_option dmux#unaligned (fun un ->
                let files =
                  Data_access.user_file_paths ~unaligned:un ~submission:sub lib in
                while_sequential files#fastx (fun (r1, r2) ->
                  while_sequential (r1 @ r2) (fun fastx_read ->
                    rendered_fastx_table fastx_read#path
                    >>= begin function
                    | Some fastx_table ->
                      fastx_quality_plots fastx_read#path >>= fun fastx_qplot ->
                      return [
                        strongf "%s: " fastx_read#kind;
                        div fastx_table;
                        div fastx_qplot;
                      ]
                    | None -> return []
                    end))
                >>| List.concat
                >>= fun fastx_items ->
                let details =
                  let li_items =
                    List.filter_map fastx_items
                      (function [] -> None | l -> Some (li l)) in
                  if li_items = []
                  then div [strongf "No Fastx information."]
                  else div [strongf "Fastx Quality Stats:"; div [ul li_items]]
                in
                return details)
              >>= fun stats ->
              let path, host=
                Option.value_map ~default:("NO-DIR", "NO-CLUSTER")
                  ~f:(fun d-> (d#directory, d#host)) del#client_fastqs_dir in
              return (content_paragraph [
                strong [pcdata "Delivered on ";
                        html_of_cluster host;
                        pcdata ": ";
                        codef "%s" path];
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




let libraries ~showing work_started info_got info =
  let open Html5 in

  let libraries_table = libraries_table info in
  let table_generated = Time.now () in
  benchmarks work_started info_got table_generated info >>= fun benchmarks ->
  Authentication.authorizes (`view (`libraries_detailed_fastq_information))
  >>= fun can_view_fastq_details ->
  (if List.length info#libraries = 1 || List.exists showing ((=) `details)
   then if can_view_fastq_details
     then per_lirbary_details info
     else per_lirbary_simple_details info
   else return (Template.content_list []))
  >>= fun details ->
  Web_data_access.classy_cache ()
  >>= fun classy_cache ->
  Authentication.user_logged ()
  >>| Option.map ~f:(fun ul -> ul.Authentication.person)
  >>= fun user_opt ->
  let libnb = List.length info#libraries in
  return Template.(
    content_section (pcdataf "Viewing %d Librar%s" libnb
                       (if libnb = 1 then "y" else "ies"))
      (content_list [
        benchmarks;
        content_section (pcdataf "Summary Table")
          (content_list [
            content_paragraph (intro_paragraph info);
            libraries_table ~classy_cache ~user_opt ~showing ~can_view_fastq_details;
           ]);
        details;
      ]))

let filter_classy_libraries_information
    ~exclude ~qualified_names ~configuration ~people_filter info =
  while_sequential info#libraries ~f:(fun l ->
    let qualified = qualified_name l#stock#project l#stock#name in
    if not (List.exists exclude ((=) qualified))
      && (qualified_names = [] ||
          List.exists qualified_names ~f:(fun qn -> qn = qualified))
    then begin
      let people =
        List.map l#submissions (fun sub -> sub#lane#contacts)
        |> List.concat
        |> List.map ~f:(fun p -> p#g_pointer) in
      people_filter people
      >>= function
      | true -> return (Some l)
      | false -> return None
    end
    else return None)
  >>| List.filter_opt
  >>= fun filtered ->
  let filtered_time = Time.now () in
  return (object
    method static_info = info
    method filtered_on = filtered_time
    method configuration = configuration
    method qualified_names = qualified_names
    method libraries = filtered
  end)

let make ~configuration =
  let people_filter people =
    Authentication.authorizes (`view (`libraries_of people)) in
  (fun (showing, qualified_names) () ->
    let work_started = Time.now () in
    let main_title = "Libraries" in
    Template.default ~title:main_title
      (Authentication.authorizes (`view `libraries)
       >>= function
       | true ->
         Template.make_content ~configuration ~main_title (
           Authentication.authorizes (`view `phix_details)
           >>= begin function
           | true -> return []
           | false -> return ["PhiX_v2"; "PhiX_v3"]
           end
           >>= fun exclude ->
           Web_data_access.classy_cache ()
           >>= fun cache ->
           let info = cache#classy_libraries in
           filter_classy_libraries_information
             ~exclude ~people_filter ~configuration ~qualified_names info
           >>= fun filtered_info ->
           let info_got = Time.now () in
           let showing = if showing = [] then [`fastq] else showing in
           Authentication.spy_userf "/libraries: [%s] Showing [%s]"
             (String.concat ~sep:", " qualified_names)
             (List.map showing Services.string_of_libraries_show
              |> String.concat ~sep:", ")
           >>= fun () ->
           libraries ~showing work_started info_got filtered_info)
       | false ->
         Template.make_authentication_error ~configuration ~main_title
           (return [Html5.pcdataf "You may not view the libraries."])))
