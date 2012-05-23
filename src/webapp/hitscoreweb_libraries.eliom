
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
  [Template.a_link Services.libraries [Html5.pcdata qn] (showing, [qn])]

let make_classy_information ~configuration ~dbh =
  let layout = Classy.make dbh in
  layout#person#all >>= fun persons ->
  layout#flowcell#all >>= of_list_sequential ~f:(fun fc ->
    of_list_sequential (Array.to_list fc#lanes) (fun lp ->
      lp#get >>= fun lane ->
      of_list_sequential (Array.to_list lane#contacts) (fun c ->
        List.find_exn persons ~f:(fun p -> p#g_id = c#id) |! return)
      >>= fun contacts ->
      of_list_sequential (Array.to_list lane#libraries) (fun l -> l#get)
      >>= fun libs ->
      return (object method lane = lane
                     method inputs = libs
                     method contacts = contacts end))
    >>= fun lanes ->
    return (object method flowcell = fc method lanes = lanes end))
  >>= fun flowcells ->
  layout#sample#all >>= fun samples ->
  layout#organism#all >>= fun organisms ->
  layout#custom_barcode#all >>= fun custom_barcodes ->
  layout#stock_library#all
  >>= of_list_sequential ~f:(fun sl ->
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
        |! Option.map ~f:(fun (idx, l) -> (fc, idx + 1, l))) in
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
    return (object method stock = sl
                   method submissions = submissions
                   method sample = sample
                   method barcoding = barcoding
                   method preparator = preparator end))
  >>= fun libraries ->
  let created = Time.now () in
  return (object (self)
    method created_on = created 
    method configuration = configuration
    method libraries = libraries
  end)

let filter_classy_information  
    ~qualified_names ~configuration ~showing info =
  of_list_sequential info#libraries ~f:(fun l ->
    if qualified_names = []
    || List.exists qualified_names
      ~f:(fun qn -> qn = qualified_name l#stock#project l#stock#name) then
      (let people =
         List.map l#submissions (fun (fc, idx, l) -> l#contacts)
         |! List.concat
         |! List.map ~f:(fun p -> p#g_pointer) in
       Authentication.authorizes (`view (`libraries_of people))
       >>= function
       | true -> return (Some l)
       | false -> return None)
    else
      return None)
  >>= fun filtered ->
  let filtered_time = Time.now () in
  return (object
    method static_info = info
    method filtered_on = filtered_time
    method showing = showing
    method configuration = configuration
    method qualified_names = qualified_names
    method libraries = List.filter_opt filtered
  end)
    
let init_classy_information ~timeout ~configuration =
  let info_mem = ref None in
  let condition = Lwt_condition.create () in
  let rec update ~configuration =
    with_database ~configuration (make_classy_information ~configuration)
    >>= fun info ->
    info_mem := Some info;
    Lwt_condition.broadcast condition info;
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

let libraries_table info =
  let open Template in
  let open Html5 in
  let everywhere f = ([`basic; `stock; `fastq], f) in
  let basic f = ([`basic], f) in
  let stock f = ([`stock], f) in
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
            List.map l (fun (fc, lane_nb, lane) ->
              let fcid = fc#flowcell#serial_name in
              span [
                a_link Services.flowcell [pcdata fcid] fcid;
                pcdataf " (Lane %d)"  lane_nb;
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
      stock (fun () -> cell_option lib#stock#note) 
      ]) in
  let first_row = [
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
  ] in
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
                         row "static info age" info#static_info#created_on;
                         row "info filtered" info#filtered_on;
                         row "info_got" info_got;
                         row "table_generated" table_generated;
                       ]))
  ) else
    return Template.(content_list [])
      
let libraries work_started info_got info =
  let open Html5 in

  let libraries_table = libraries_table info in
  let table_generated = Time.now () in
  benchmarks work_started info_got table_generated info >>= fun benchmarks ->
  return Template.(
    content_section (pcdataf "Viewing %d Libraries" (List.length info#libraries))
      (content_list [
        content_paragraph (intro_paragraph info);
        benchmarks;
        libraries_table;
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
