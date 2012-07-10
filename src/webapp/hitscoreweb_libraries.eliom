
open Hitscoreweb_std
open Hitscore

module Msg = Hitscoreweb_messages
  
module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

module Persons_service = Hitscoreweb_persons

module Data_access = Hitscoreweb_data_access

let qualified_name po n =
  sprintf "%s%s" Option.(value_map ~default:"" ~f:(sprintf "%s.") po) n

let qualified_link ~showing po n =
  let qn = qualified_name po n in
  [Template.a_link Services.libraries [Html5.pcdata qn] (showing, [qn])]

let filter_map l ~filter ~map =
  List.filter_map l ~f:(fun x -> if filter x then Some (map x) else None)
let get_by_id l i = List.find l (fun o -> o#g_id = i)
let getopt_by_id l i =
  List.find l (fun o -> Some o#g_id = Option.map i (fun i -> i#id))
    
let rec volume_path ~configuration vols v =
  let open Layout.File_system in
  let open Option in
  Configuration.path_of_volume_fun configuration >>= fun path_fun ->
  begin match v#g_content with
  | Tree (hr_tag, trees) ->
    let vol = Common.volume_unix_directory ~id:v#g_id ~kind:v#g_kind ?hr_tag in 
    return (path_fun vol)
  | Link p ->
    (get_by_id vols p.id
     >>= fun vv ->
     volume_path ~configuration vols vv)
  end

let make_classy_information ~configuration ~dbh =
  let creation_started_on = Time.now () in
  let layout = Classy.make dbh in
  layout#hiseq_raw#all >>= fun hiseq_raws ->
  layout#assemble_sample_sheet#all >>= fun ssassemblies ->
  layout#sample_sheet#all >>= fun sample_sheets ->
  layout#bcl_to_fastq#all >>= fun b2fs ->
  layout#bcl_to_fastq_unaligned#all >>= fun b2fus ->
  layout#file_system#all >>= fun volumes ->
  layout#prepare_unaligned_delivery#all >>= fun udeliveries ->
  layout#client_fastqs_dir#all >>= fun client_dirs ->
  layout#generic_fastqs#all >>= fun generic_fastqs ->
  layout#fastx_quality_stats#all >>= fun fxqss ->
  layout#fastx_quality_stats_result#all >>= fun fxqs_rs ->
  layout#person#all >>= fun persons ->

  while_sequential b2fs (fun b2f ->
    let b2fu = getopt_by_id b2fus b2f#g_result in
    let hiseq_raw = get_by_id hiseq_raws b2f#raw_data#id in
    let vol =
      Option.(b2fu >>= fun b2fu ->
              hiseq_raw >>= fun hiseq_raw ->
              get_by_id volumes b2fu#directory#id
              >>= fun vol ->
              volume_path ~configuration volumes vol
              >>= fun path ->
              Some (vol , hiseq_raw, path, b2fu)) in
    map_option vol (fun (vol, hiseq_raw, path, b2fu) ->
      let basecall_stats_path =
        Filename.concat path
          (sprintf "Unaligned/Basecall_Stats_%s/%s"
             hiseq_raw#flowcell_name "Flowcell_demux_summary.xml") in
      let dmux_summary_m =
        Data_access.File_cache.get_demux_summary basecall_stats_path in
      double_bind dmux_summary_m
        ~ok:(fun dmux_summary -> return (Some dmux_summary))
        ~error:(fun e ->
          (* eprintf "Error getting: %s\n%!" basecall_stats_path; *)
          return None)
      >>= fun dmux_summary ->
      let generic_vols =
        List.filter volumes (fun v ->
          v#g_content = Layout.File_system.Link vol#g_pointer)
      in
      let generics = 
        List.map generic_vols (fun gv ->
          List.filter generic_fastqs (fun gf ->
            gf#directory#id = gv#g_id)) |! List.concat in
      let fastx_qss =
        List.map generics (fun g ->
          List.filter fxqss (fun f -> f#input_dir#id = g#g_id)) |! List.concat in
      let fastx_results =
        List.filter_map fastx_qss (fun fxqs ->
          getopt_by_id fxqs_rs fxqs#g_result) in
      let fastx_paths =
        List.filter_map fastx_results (fun fr ->
          let open Option in
          get_by_id volumes fr#directory#id
          >>= volume_path ~configuration volumes) in
          
      return (object
        method vol = vol
        method path = path
        method dmux_summary = dmux_summary
        method hiseq_raw = hiseq_raw
        method b2fu = b2fu
        method generic_vols = generic_vols
        method generics = generics
        method fastx_qss = fastx_qss
        method fastx_results = fastx_results
        method fastx_paths = fastx_paths
      end)))
  >>| List.filter_opt
  >>= fun unaligned_volumes ->
  
  layout#flowcell#all >>= while_sequential ~f:(fun fc ->
    while_sequential (Array.to_list fc#lanes) (fun lp ->
      lp#get >>= fun lane ->
      while_sequential (Array.to_list lane#contacts) (fun c ->
        List.find_exn persons ~f:(fun p -> p#g_id = c#id) |! return)
      >>= fun contacts ->
      while_sequential (Array.to_list lane#libraries) (fun l -> l#get)
      >>= fun libs ->
      return (object method oo = lane
                     method inputs = libs
                     method contacts = contacts end))
    >>= fun lanes ->
    let hiseq_raws =
      filter_map hiseq_raws
        ~filter:(fun h -> h#flowcell_name = fc#serial_name)
        ~map:(fun h ->
          let demuxes =
            filter_map b2fs ~filter:(fun b -> b#raw_data#id = h#g_id)
              ~map:(fun b2f ->
                let unaligned =
                  Option.(getopt_by_id b2fus b2f#g_result
                          >>= fun u ->
                          List.find unaligned_volumes (fun uv ->
                            uv#vol#g_id = u#directory#id)) in
                let sample_sheet = get_by_id sample_sheets b2f#sample_sheet#id in
                let assembly =
                  Option.(sample_sheet >>= fun s ->
                          List.find_map ssassemblies (fun a ->
                            a#g_result >>= fun r ->
                            if r#id = s#g_id then return a else None)) in
                let deliveries =
                  Option.map unaligned ~f:(fun unalig ->
                    filter_map udeliveries
                      ~filter:(fun u -> u#unaligned#id = unalig#b2fu#g_id)
                      ~map:(fun u ->
                        let dir = getopt_by_id client_dirs u#g_result in
                        (object method oo = u (* delivery *)
                                method client_fastqs_dir = dir end))) in
                (object
                  method b2f = b2f (* demultiplexing *)
                  method sample_sheet = sample_sheet
                  method assembly = assembly
                  method unaligned = unaligned
                  method deliveries = Option.value ~default:[] deliveries
                 end)) in
          (object (* hiseq_raw *)
            method oo = h method demultiplexings = demuxes end))
    in
    return (object method oo = fc (* Flowcell *)
                   method lanes = lanes
                   method hiseq_raws = hiseq_raws end))
  >>= fun flowcells ->
  layout#sample#all >>= fun samples ->
  layout#organism#all >>= fun organisms ->
  layout#custom_barcode#all >>= fun custom_barcodes ->
  layout#invoicing#all >>= fun invoices ->
  layout#stock_library#all
  >>= while_sequential ~f:(fun sl ->
    let optid =  Option.map ~f:(fun o -> o#id) in
    let sample =
      List.find_map samples (fun s ->
        if Some s#g_id = optid sl#sample then (
          let org = List.find organisms (fun o -> Some o#g_id = optid s#organism) in
          Some (object method sample = s method organism = org end))
        else None) in
    let submissions =
      List.filter_map flowcells (fun fc ->
        List.findi fc#lanes ~f:(fun idx l ->
          List.exists l#inputs (fun i -> i#library#id = sl#g_id))
        |! Option.map ~f:(fun (idx, l) ->
          let invoices =
            List.filter invoices (fun i ->
              Array.exists i#lanes (fun ll -> ll#id = l#oo#g_id)) in
          (object method flowcell = fc (* submission *)
                  method lane_index = idx + 1
                  method lane = l
                  method invoices = invoices
           end))) in
    let preparator =
      List.find persons (fun p ->
        Some p#g_pointer = Option.map sl#preparator (fun p -> p#pointer)) in
    let barcoding =
      let barcodes = Array.to_list sl#barcodes in
      match sl#barcode_type with
      | `none -> `None
      | `bioo -> `Bioo barcodes
      | `bioo_96 -> `Bioo_96 barcodes
      | `illumina -> `Illumina barcodes
      | `nugen -> `Nugen barcodes
      | `custom ->
        let barcodes =
          List.filter custom_barcodes (fun cb ->
            Array.exists sl#custom_barcodes (fun b -> b#id = cb#g_id)) in
        `Custom barcodes in
    return (object method stock = sl (* library *)
                   method submissions = submissions
                   method sample = sample
                   method barcoding = barcoding
                   method preparator = preparator end))
  >>= fun libraries ->
  let created = Time.now () in
  logf "{libraries} Static info updated" >>= fun () ->
  return (object (self)
    method creation_started_on = creation_started_on
    method created_on = created 
    method configuration = configuration
    method libraries = libraries
  end)

let filter_classy_information  
    ~qualified_names ~configuration ~showing info =
  while_sequential info#libraries ~f:(fun l ->
    if qualified_names = []
    || List.exists qualified_names
      ~f:(fun qn -> qn = qualified_name l#stock#project l#stock#name) then
      (let people =
         List.map l#submissions (fun sub -> sub#lane#contacts)
         |! List.concat
         |! List.map ~f:(fun p -> p#g_pointer) in
       Authentication.authorizes (`view (`libraries_of people))
       >>= function
       | true -> return (Some l)
       | false -> return None)
    else
      return None)
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
    method libraries = List.filter_opt filtered
    method can_view_fastq_details = can_view_fastq_details
  end)
    
let init_classy_information ~timeout ~configuration =
  let info_mem = ref None in
  let condition = Lwt_condition.create () in
  let rec update ~configuration =
    let m =
      with_database ~configuration (make_classy_information ~configuration)
      >>= fun info ->
      info_mem := Some info;
      Lwt_condition.broadcast condition info;
      return ()
    in
    double_bind m
      ~ok:return
      ~error:(fun e ->
        wrap_io Lwt_io.eprintf "Updating the classy info gave an error.\n"
        >>= fun () ->
        return ())
    >>= fun () ->
    wrap_io Lwt_unix.sleep timeout
    >>= fun () ->
    update ~configuration in
  Lwt.ignore_result (update ~configuration);
  fun ~qualified_names ~showing ->
    begin
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
    List.map [ [`basic], "Basic"; [`fastq], "Fastq Info"; [`stock], "Stock Info";
               [`basic;`stock; `fastq], "Full" ] make_link
    |! interleave_list ~sep:(pcdata ", ") in
  [pcdata "Choose view: ";] @ links @ [pcdata "."]

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
  `subtable (if subtable = [] then empty_row else subtable)

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
  `subtable (if subtable = [] || subtable = [[]] then empty_row else subtable)

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
        `sortable (qualified_name lib#stock#project lib#stock#name,
                   qualified_link ~showing:info#showing
                     lib#stock#project lib#stock#name));
      basic (fun () -> cell_option lib#stock#description);
      basic (fun () -> cell_option (Option.map lib#sample (fun s -> s#sample#name)));
      basic (fun () ->
        cell_option Option.(lib#sample >>= fun s -> s#organism >>= fun o -> o#name));
      basic (fun () ->
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
      basic (fun () -> cell_option lib#stock#application);
      stock (fun () ->
        let strlist l = String.concat ~sep:"," (List.map l ~f:(sprintf "%d")) in
        match lib#barcoding with
        | `None              -> cell_fmt "NONE"
        | `Bioo barcodes     -> cell_fmt "BIOO[%s]" (strlist barcodes)
        | `Bioo_96 barcodes  -> cell_fmt "BIOO-96[%s]" (strlist barcodes) 
        | `Illumina barcodes -> cell_fmt "Illumina[%s]" (strlist barcodes)
        | `Nugen barcodes    -> cell_fmt "NuGen[%s]" (strlist barcodes)
        | `Custom barcodes   ->
          cell_fmt "Custom[%s]" (List.map barcodes (fun b ->
            sprintf "(%s%s%s%s)" b#sequence
              Option.(value_map b#position_in_r1 ~default:"" ~f:(sprintf "-R1:%d"))
              Option.(value_map b#position_in_r2 ~default:"" ~f:(sprintf "-R2:%d"))
              Option.(value_map b#position_in_index ~default:"" ~f:(sprintf "-I:%d"))
          ) |! String.concat ~sep:","));
      stock (fun () -> cell_int_option lib#stock#p5_adapter_length);
      stock (fun () -> cell_int_option lib#stock#p7_adapter_length);
      stock (fun () -> cell_text (Bool.to_string lib#stock#stranded));
      stock (fun () -> cell_text (Bool.to_string lib#stock#truseq_control));
      stock (fun () -> cell_option lib#stock#rnaseq_control);
      stock (fun () ->
        Option.value_map lib#preparator ~default:(`text []) ~f:(fun p ->
          `sortable (p#email,
                     [a_link Services.persons [pcdata p#email] (None, [p#email])])));
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
      everywhere (fun () -> `head_cell Msg.library_qn);
      basic (fun () -> `head_cell Msg.library_description);
      basic (fun () -> `head_cell Msg.sample_name);
      basic (fun () -> `head_cell Msg.organism_name);
      basic (fun () -> `head_cell Msg.library_submissions);
      basic (fun () -> `head_cell Msg.library_application);
      stock (fun () -> `head_cell Msg.library_barcode);
      stock (fun () -> `head_cell Msg.library_p5);
      stock (fun () -> `head_cell Msg.library_p7);
      stock (fun () -> `head_cell Msg.library_stranded);
      stock (fun () -> `head_cell Msg.library_truseq_control);
      stock (fun () -> `head_cell Msg.library_rnaseq_control);
      stock (fun () -> `head_cell Msg.library_preparator);
      stock (fun () -> `head_cell Msg.library_note);
      fastq (fun () -> `head_cell Msg.library_submissions);
    ] @ fastq_part
  in
  let table =
    List.map (first_row :: rows) ~f:(fun row ->
      List.filter_map row (fun (where, what) ->
        if List.exists where ~f:(fun w -> List.exists info#showing ((=) w))
        then Some (what ()) else None)) in
  
  Template.content_table table
    
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
  Data_access.File_cache.(
    get_fastx_quality_stats path
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
    let open Data_access.File_cache in
    get_fastx_quality_stats path
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
    | `Illumina b ->
      List.filter_map b (fun i ->
        List.Assoc.find Assemble_sample_sheet.illumina_barcodes i)
    | `Bioo b ->
      List.filter_map b (fun i ->
        List.Assoc.find Assemble_sample_sheet.bioo_barcodes i)
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
  

let make ~timeout ~configuration =
  let classy_info = init_classy_information ~timeout ~configuration in
  (fun (showing, qualified_names) () ->
    let work_started = Time.now () in
    let main_title = "Libraries" in
    Template.default ~title:main_title
      (Authentication.authorizes (`view `libraries)
       >>= function
       | true ->
         Template.make_content ~configuration ~main_title (
           with_database ~configuration (fun ~dbh ->
             classy_info ~qualified_names ~showing
             >>= fun info ->
             let info_got = Time.now () in
             libraries work_started info_got info))
       | false ->
         Template.make_authentication_error ~configuration ~main_title
           (return [Html5.pcdataf "You may not view the libraries."])))
