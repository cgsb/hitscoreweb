{shared{
open Hitscoreweb_std
}}

module Msg = Hitscoreweb_messages

module Web_data_access = Hitscoreweb_data_access

module Queries = Hitscoreweb_queries

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

module Layout_service = Hitscoreweb_layout_service

module One_person_service = Hitscoreweb_one_person_service

module Persons_service = Hitscoreweb_persons


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


end



module Evaluations_service = struct

  let b2f_section dbh layout =
    layout#assemble_sample_sheet#all >>= fun all_assemblies ->
    layout#bcl_to_fastq#all
    >>| List.stable_sort ~cmp:(fun a b -> compare b#g_inserted a#g_inserted)
    >>= fun b2fs ->
    while_sequential b2fs ~f:(fun b2f ->
      b2f#raw_data#get >>= fun hiseq_raw ->
      let assembly =
        List.find all_assemblies ~f:(fun g ->
          match g#g_result with
          | None -> false
          | Some r -> r#id = b2f#sample_sheet#id) in
      return (b2f, hiseq_raw, assembly))
    >>= fun b2fs ->
    while_sequential b2fs (fun (b2f, hiseq_raw, assembly) ->
      let open Template in
      return [
        cell_int b2f#g_id;
        cell_text (Sql_query.status_to_string b2f#g_status);
        cell_timestamp b2f#g_inserted;
        cell_timestamp_option b2f#g_started;
        cell_timestamp_option b2f#g_completed;
        cell_text hiseq_raw#flowcell_name;
        cell_int b2f#mismatch;
        cell_text b2f#version;
        cell_option b2f#tiles;
        cell_option b2f#bases_mask;
        cell_option
          Option.(map assembly
                    ~f:(fun a ->
                      Layout.Enumeration_sample_sheet_kind.to_string a#kind));
      ])
    >>= fun rows ->
    return Template.(
      let tab =
        content_table (
          [`head [Html5.pcdata "Id"];
           `head [Html5.pcdata "Status"];
           `head [Html5.pcdata "Inserted"];
           `head [Html5.pcdata "Started"];
           `head [Html5.pcdata "Completed"];
           `head [Html5.pcdata "Flowcell"];
           `head [Html5.pcdata "Mismatch"];
           `head [Html5.pcdata "Version"];
           `head [Html5.pcdata "Tiles Option"];
             `head [Html5.pcdata "Bases-Mask Option"];
             `head [Html5.pcdata "Kind of sample-sheet"];
            ] :: rows)
        in
        content_section (Html5.pcdataf "Bcl_to_fastq evaluations: %d"
                           (List.length b2fs)) tab)

  let fxqs_section dbh layout =
    layout#fastx_quality_stats#all
    >>| List.stable_sort ~cmp:(fun a b -> compare b#g_inserted a#g_inserted)
    >>= fun fxqss ->
    while_sequential fxqss ~f:(fun fxqs ->
      fxqs#input_dir#get >>= fun gf ->
      let configuration = Configuration.configure ~root_path:"root:/" () in
      Common.path_of_volume ~configuration ~dbh gf#directory#pointer
      >>= fun path ->
      return (path, fxqs))
    >>= while_sequential ~f:(fun (path, fxqs) ->
      let open Template in
      return [
        cell_int fxqs#g_id;
        cell_text (Sql_query.status_to_string fxqs#g_status);
        cell_timestamp fxqs#g_inserted;
        cell_timestamp_option fxqs#g_started;
        cell_timestamp_option fxqs#g_completed;
        cell_text path;
        cell_int fxqs#option_Q;
        cell_option fxqs#filter_names;
      ])
    >>= fun rows ->
    return Template.(
      let tab =
        content_table (
          [`head [Html5.pcdata "Id"];
           `head [Html5.pcdata "Status"];
           `head [Html5.pcdata "Inserted"];
           `head [Html5.pcdata "Started"];
           `head [Html5.pcdata "Completed"];
           `head [Html5.pcdata "Input path"];
           `head [Html5.pcdata "Option Q"];
           `head [Html5.pcdata "Filter Names"];
          ] :: rows)
        in
        content_section (Html5.pcdataf
                           "Fastx_quality_stats evaluations: %d"
                           (List.length fxqss)) tab)

  let evaluations configuration =
    with_database ~configuration (fun ~dbh ->
      let layout = Classy.make dbh in
      b2f_section dbh layout >>= fun b2f ->
      fxqs_section dbh layout >>= fun fxqs ->
      return Template.(content_list [b2f; fxqs])
    )

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


module Test_service = struct

  let test_form ~state =
    let open Hitscoreweb_meta_form in
    let upload_many_files_store = Upload.fresh_store [] in
    let upload_one_file_store = Upload.fresh_store [] in
    let services_form =
      ref (
        let identifier_friendly =
          ("non-empty string of letters, numbers, underscores, and dashes",
           "[a-zA-Z0-9_-]+") in
        let strictly_positive = Range.(make (exclusive 0.) infinity) in
        let percentage = Range.(make (inclusive 1.) (inclusive 100.)) in
        let open Form in
        make ~text_buttons:["Submit …"; "Cancel"; "Trigger Error"]
          (section Markup.([text "First Section"]) [
            integer ~question:[Markup.text "Pick an integer"] ~value:42 ();
            (let question =
               Markup.([text "Pick a string "; italic "(regular expression)"]) in
             string ~regexp:identifier_friendly ~question ());
            section Markup.([text "Subsection"]) [
              string
                ~help:Markup.(par [text "HHEEEELLLPPP"])
                ~text_question:"Pick a string" ~value:"sldk jskd" ();
              date ~text_question:"A date:" ~value:"21/12/2012" ();
              date ~text_question:"Another date:" ();
              upload ~store:upload_many_files_store
                Markup.([text "Upload many FILEs ! "; italic "pleaaase"]);
              upload ~store:upload_one_file_store  ~multiple:false
                Markup.([text "Upload one FILE ! "; italic "pleaaase"]);
              float  ~text_question:"Now a float:" ~value:(atan (-1.)) ();
              float  ~text_question:"percent float" ~range:percentage ();
              float  ~text_question:"float > 0." ~range:strictly_positive ();
              integer ~text_question:"int > 0" ~range:strictly_positive (); 
              string_enumeration ~question:Markup.([text "Many strings?"])
                ~value:"one" ["zero"; "one"; "two"; "three"];
              open_string_enumeration ~question:Markup.([text "Many strings?"])
                ~value:"one" ~other:"Make up another one …"
                ["zero"; "one"; "two"; "three"];
              begin
                let make_sub ?name ?age () =
                  section Markup.([text "Create a new person"]) [
                    string  ~text_question:"person's name" ?value:name ();
                    integer ~text_question:"person's age" ?value:age ();
                  ] in
                meta_enumeration
                  ~help:Markup.(list [par [text "some help"]; par [italic "more help"]])
                  ~overall_question:Markup.([text "Please choose or create a person"])
                  ~creation_cases:[
                    ("Create person …", make_sub ());
                    ("Create ageless person …",
                     section Markup.([text "Create an ageless person"]) [
                       string  ~text_question:"person's name" ~value:"LA Woman" ();
                     ]);
                  ]
                  ~choice:"the first"
                  [("the first", make_sub ~name:"The First" ~age:42 ());
                   ("anotherone", make_sub ~name:"Another One" ~age:45 ());
                   ("notthefirst", make_sub ~name:"Not The First" ~age:22 ());]
              end;
              section Markup.([text "Extensible List:"]) [begin
                let make_model sec =
                  section Markup.([ text sec ]) [
                    string ~text_question:"Pick a name" ();
                    string_enumeration ~question:Markup.([text "Pick a ";
                                                          italic "type:"])
                      ["int"; "float"];
                  ] in
                extensible_list ~question:Markup.([italic "Add a thing"])
                  ~model:(make_model "New thing")
                  (List.init 2 (ksprintf make_model "%d one"))
              end];
            ];
          ])
      ) in 
    create ~state
      Form.(function
      | None ->
        return (make ~text_buttons:["Start the form"] empty)
      | Some {form_content = Empty; _} ->
        return (!services_form)
      | Some ({form_content = (Section (title, modified_form));} as form)
          when title = [Markup.text "First Section"] ->
        begin match form.form_choice with
        | Some 0 ->
          services_form := `form form;
          dbg "Modified form : %s"
            Deriving_Json.(to_string Json.t<Hitscoreweb_meta_form.form_content> modified_form);
          dbg "Files:[\n TODO \n]";
          return (make ~text_buttons:["Would you like to restart?"] empty)
        | Some 1 ->
          dbg "User clicked Cancel";
          return (make ~text_buttons:["Send"]
                    (string ~text_question:"Why did you cancel???" ()))
        | Some 2 ->
          dbg "User clicked to trigger an error";
          error [Markup.text "TRIGGERED ERROR !!"]
        | _ ->
          dbg "unexpected input";
          error [Markup.text "UNEXPECTED ERROR !!"]
        end
      | Some _ ->
        return (make ~text_buttons:["Nothing to save?"] empty)
      )

      (*
  type new_person = {
    name: string * string option * string;
    email: string;
    net_id: string option;
  } 
  deriving (Json)

    let look_for_new_persons = Form.(function
      | List (
        Section (msg_contacts_section, List [Extensible_list el])
        :: _) ->
        List.filter_map el.el_list (function
        | Meta_enumeration {choice = Some chosen; creation_cases} ->
          List.Assoc.find creation_cases chosen
        | _ -> None)
      | _ -> assert false
        )
      *)
  type user_submission_form = {
    persons_form: Hitscoreweb_meta_form.form option;
    libraries_form: Hitscoreweb_meta_form.form option;
    created: Time.t;
    last_modified: Time.t;
  }

  let _temporary_form_store = (ref [] : (int * int * user_submission_form) list ref)

  let forms_of_user id =
    List.filter_map !_temporary_form_store
      (fun (u, k, f) -> if u = id then Some (k, f) else None)
    |! return

  let find_form form_list ~key = List.Assoc.find form_list key

  let user_persons_form user_id key_opt = 
    match key_opt with
    | Some key ->
      forms_of_user user_id >>= fun forms ->
      begin match (find_form forms key) with
      | Some { persons_form  } -> return (persons_form)
      | _ -> return None
      end
    | None -> return None

  let user_libraries_form user_id key_opt = 
    match key_opt with
    | Some key ->
      forms_of_user user_id >>= fun forms ->
      begin match (find_form forms key) with
      | Some { libraries_form  } -> return (libraries_form)
      | _ -> return None
      end
    | None -> return None

  let save_forms user key_opt form_to_save  =
    match List.find !_temporary_form_store (fun (u, k, _) -> u = user && Some k = key_opt) with
    | Some (u, k, sub) ->
      dbg "Saving form %d for user: %d" k u;
      let to_save =
        match form_to_save with
        | `persons f ->
          { sub with persons_form = Some f; last_modified = Time.now () }
        | `libraries f ->
          { sub with libraries_form = Some f; last_modified = Time.now () }
      in
      _temporary_form_store :=
        List.map !_temporary_form_store (fun (uu, kk, f) ->
          if u = uu && k = kk then (u, k, to_save) else (uu, kk, f));
      return ()
    | None ->
      let new_key =
        (List.fold !_temporary_form_store ~init:0 ~f:(fun m (_,k,_) -> max m k))
        + 1 in
      let persons_form, libraries_form =
        match form_to_save with
        | `persons p -> Some p, None
        | `libraries l -> None, Some l in
      let to_save = { persons_form; libraries_form;
                      created = Time.now (); last_modified = Time.now () } in
      dbg "Adding submission %d (for persons) for user: %d" new_key user;
      _temporary_form_store := (user, new_key, to_save) :: !_temporary_form_store;
      return ()

  let save_persons_form user key_opt form =
    save_forms user key_opt (`persons form)
  let save_libraries_form user key_opt form =
    save_forms user key_opt (`libraries form)

  let delete_submission user key  =
    logf "Deleting submission %d of user %d" key user >>= fun () ->
    _temporary_form_store :=
      List.filter !_temporary_form_store (fun (u, k, _) ->
        not (u = user && k = key));
    return ()
      
  module Msg = struct

    open Hitscoreweb_meta_form

    let start_a_new_submission = [Markup.text "Start a new submission"]
    let edit_contacts = [Markup.text "Edit contacts"]
    let edit_libraries = [Markup.text "Edit libraries"]
    let delete_submission = [Markup.text "Delete this submission"]

    (* let TODO = [Markup.text "TODO"] *)
    let save = [Markup.text "Save"]
    let cancel = [Markup.text "Cancel"]
    let error s = [ksprintf Markup.text "UNEXPECTED ERROR: %s !!" s]

    let contacts_section = [Markup.text "Contacts"]
    let add_contact = [Markup.text "Add a contact"]
    let choose_contact = [Markup.text "Pick a user or create a new one:"]
    let create_new_user =  "Create a new user …"
    let given_name = "Given name (mandatory)"
    let middle_name = "Middle name"
    let family_name = "Family name (mandatory)"
    let net_id = "Net ID"
    let email = "Email address (mandatory)"

    let libraries_section = [Markup.text "Libraries"]
    let add_library = [Markup.text "Add a library"]
    let choose_or_create_library = 
      [Markup.text "Pick an existing library (of yours) or define a new one:"]
    let create_new_library = "Create a new library …"
    let library_name = "Library Name (mandatory)"

  end
    
  module Regexp = struct
    let mandatory_identifier =
      ("non-empty string of letters, numbers, underscores, and dashes",
       "[a-zA-Z0-9_-]+")
    let mandatory_string = ("non-empty string", ".+")
    let mandatory_email =
      ("valid email address", "[a-zA-Z0-9_-@\\.\\+]+")
    let optional_net_id = ("NYU Net ID", "[a-z0-9]*")
  end

  let contacts_form_key = "contacts"
  let new_contacts_form ~state =
    let open Hitscoreweb_meta_form in
    let open Form in
    Hitscoreweb_state.persons_info state
    >>= fun persons_info ->
    let all_contacts =
      List.map persons_info#persons (fun p ->
        (sprintf "%s, %s &lt;<code>%s</code>&gt;"
           p#t#family_name p#t#given_name p#t#email,
         string ~value:p#t#email ())) in
    let contacts_section =
      let make_model v = 
        let choice = Option.value ~default:"" v in
        let open Regexp in
        meta_enumeration 
          ~overall_question:Msg.choose_contact
          ~creation_cases:[
            (Msg.create_new_user, list [
              string ~text_question:Msg.given_name ~regexp:mandatory_string ();
              string ~text_question:Msg.middle_name ();
              string ~text_question:Msg.family_name ~regexp:mandatory_string ();
              string ~text_question:Msg.email ~regexp:mandatory_email ();
              string ~text_question:Msg.net_id ~regexp:optional_net_id ();
            ]);
          ]
          ~choice
            (("", empty) :: all_contacts)
        in
        section Msg.contacts_section [
          extensible_list ~question:Msg.add_contact
            ~model:(make_model None) []
        ] in
    return (make ~buttons:[Msg.save; Msg.cancel]
              ~key:contacts_form_key contacts_section)

  let libraries_form_key = "libraries"
  let new_libraries_form ~state user_id =
    let open Hitscoreweb_meta_form in
    let open Form in
    Hitscoreweb_state.persons_info state
    >>= fun persons_info ->
    let all_libraries =
      List.find_map persons_info#persons (fun p ->
        if p#t#g_id = user_id
        then Some (List.map p#libraries (fun l ->
          let qn = 
            sprintf "%s%s"
              (Option.value_map l#project ~default:"" ~f:(sprintf "%s."))
              l#name in
          (sprintf "<code>%s</code>" qn, string ~value:qn ()))
          |! List.dedup |! List.sort ~cmp:compare)
        else None)
      |! Option.value ~default:[]
    in
    let libraries_section =
      let model =
        meta_enumeration 
          ~overall_question:Msg.choose_or_create_library
          ~creation_cases:[
            (Msg.create_new_library, list [
              string ~text_question:Msg.library_name
                ~regexp:Regexp.mandatory_identifier ();
            ]);
          ]
          ~choice:""
          (("", empty) :: all_libraries)
      in
      section Msg.libraries_section [
        extensible_list ~question:Msg.add_library ~model []
      ] in
    return (make ~buttons:[Msg.save; Msg.cancel]
              ~key:libraries_form_key libraries_section)

  let submission_form ~state user_id form_key_opt =
    let open Hitscoreweb_meta_form in
    let open Form in
    let wrap_error m =
      m >>< begin function
      | Ok o -> return o
      | Error (_) ->
        error (Msg.error "STATE/DATABASE ERROR")
      end in
    let initial_action_buttons, initial_action_buttons_handler =
      let buttons =
        [Msg.edit_contacts; Msg.edit_libraries; Msg.delete_submission] in
      let handler ~when_contacts ~when_libraries ~when_delete choice =
        wrap_error
          begin match choice with
          | Some 0 -> when_contacts ()
          | Some 1 -> when_libraries ()
          | Some 2 -> when_delete ()
          | c ->
            logf "submission_form: Unexpected initial_action_button choice: \
                %s (user: %d, form-key: %s)"
              (Option.value_map ~default:"None" c ~f:(sprintf "%d"))
              user_id
              (Option.value_map ~default:"None" form_key_opt ~f:(sprintf "%d"))
            >>= fun () ->
            error (`string "wrong choice")
          end
      in
      (buttons, handler)
    in
    let start () =
      match form_key_opt with
      | None -> return (make ~buttons:[Msg.start_a_new_submission] empty)
      | Some _ ->
        return (make ~buttons:initial_action_buttons empty)
    in
    create ~state begin function
    | None -> start ()
    | Some ({form_content = Empty; _} as form) ->
      initial_action_buttons_handler form.form_choice
        ~when_contacts:(fun () ->
          user_persons_form user_id form_key_opt
          >>= begin function 
          | Some f -> return (`form f)
          | None -> new_contacts_form ~state
          end)
        ~when_libraries:(fun () ->
          user_libraries_form user_id form_key_opt
          >>= begin function 
          | Some f -> return (`form f)
            | None -> new_libraries_form ~state user_id
          end)
        ~when_delete:(fun () ->
          let key = Option.value_exn form_key_opt in
          delete_submission user_id key >>= fun () ->
          return reload)
    | Some ({form_choice = Some 0} as form) when form.form_key = Some contacts_form_key ->
      save_persons_form user_id form_key_opt form
      >>= fun () ->
      return reload
    | Some ({form_choice = Some 0} as form) when form.form_key = Some libraries_form_key ->
      save_libraries_form user_id form_key_opt form
      >>= fun () ->
      return reload
    | Some ({form_choice = Some 1}) -> start ()
    | _ -> 
      dbg "unexpected choice?";
      error (Msg.error "?")
    end

    
  let test_user = ref 0
  let pick_test_user_form ~state =
    let open Hitscoreweb_meta_form in
    let open Form in
    let contacts = ref [] in
    let simple_form reply =
      begin
        Hitscoreweb_state.persons_info state
        >>= fun persons_info ->
        let all_contacts =
          List.map persons_info#persons (fun p ->
            let choice =
              sprintf "%s, %s &lt;<code>%s</code>&gt;"
                p#t#family_name p#t#given_name p#t#email in
            contacts := (choice, p#t#g_id) :: !contacts;
            (choice, integer ~value:p#t#g_id ())) in
        return (make ~buttons:[ [Markup.text "Go"] ]
                  (section [Markup.text "Set the “test” user"] [
                    meta_enumeration ~choice:"" (("", empty) :: all_contacts);
                  ]))
      end
      >>< begin function
      | Ok o -> return o
      | Error (_) ->
        error (Msg.error "STATE/DATABASE ERROR")
      end
    in
    create ~state (function
    | None ->
      simple_form None
    | Some form ->
      begin match form.form_content with
      | Section (_, Meta_enumeration {choice; _}) ->
        dbg "Choice: %s" (Option.value ~default:"NONE" choice);
        test_user :=
          Option.(
            (choice >>= fun c ->
             List.Assoc.find !contacts c) |! value  ~default:0);
        return reload
      | f ->
        dbg " cannot parse form answer: %s"
          Deriving_Json.(to_string Json.t<Hitscoreweb_meta_form.form_content> f);
        error [
          ksprintf Markup.text " cannot parse form answer: %s"
            Deriving_Json.(to_string Json.t<Hitscoreweb_meta_form.form_content> f);
        ]
      end)
      
  let test ~state =
    let open Html5 in
    forms_of_user !test_user
    >>| List.rev_map ~f:(fun (k, f) ->
      li [p [span [pcdataf "Created on %s, last modified on %s "
                      (Time.to_string f.created)
                      (Time.to_string f.last_modified)];
             submission_form ~state !test_user (Some k)]])
    >>= fun forms ->
    let content =
      let welcome = [
        h2 [pcdata "Welcome"];
        h3 [pcdata "Test Form:"];
        p [test_form ~state];
        h3 [pcdata "Submission Forms:"];
        p [pick_test_user_form ~state];
        p [pcdataf "Submission forms as %d:" !test_user];
        ul forms;
        p [submission_form ~state !test_user None];
      ] in
      return (welcome)
    in
    content

  let make ~state =
    (fun () () ->
      let main_title = "Test" in
      Template.default ~title:main_title
        (Authentication.authorizes (`view `test_service)
         >>= function
         | true ->
           test ~state >>= fun c ->
           return c
         | false ->
           Template.make_authentication_error ~main_title
             ~configuration:state.Hitscoreweb_state.configuration
             (return [Html5.pcdataf "You may not view the tests."])))
end

module Default_service = struct

    
    
  let make ~state =
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
            pcdata "This is Gencore's LIMS web-application; for general \
                    information see ";
            core_a
              ~a:[ a_hreff "http://biology.as.nyu.edu/object/biology.facilities.sequencing" ]
              [pcdata "Gencore's webpages"];
            pcdata ". ";
          ];
        ] in
        return (header @ welcome @ menu)
      in
      Template.default ~title:"Home" content)

end

module Doc_service = struct

  let sdx_to_html document =
    let open Sequme_doc_syntax in
    let open Html5 in
    let rec inline = function
      | Bold inside -> b (List.map inside inline)
      | Italic inside -> i (List.map inside inline)
      | Code  inside -> code (List.map inside inline)
      | Link (options, inside) ->
        begin match options with
        | [] -> span (List.map inside inline)
        | `url url :: _ when String.is_prefix url ~prefix:"mailto:" ->
          let id = unique_id "aid" in
          Template.anti_spam_mailto ~id ~mailto:url;
          span [core_a ~a:[a_id id; a_hreff "ah ah no-spam"]
                   (List.map inside inline)]
        | `url url :: _ ->
          span [core_a ~a:[a_hreff "%s" url] (List.map inside inline)]
        | `local l :: _ ->
          span [Html5.a ~fragment:l ~service:Eliom_service.void_coservice'
                   (List.map inside inline) ()]
        end
      | Text s ->
        pcdata s in
    let rec structural = function
      | Section (level, idopt, inside) ->
        let a = Option.value_map idopt ~default:[] ~f:(fun id -> [a_id id]) in
        begin match level with
        | `one   -> h1 ~a (List.map inside inline)
        | `two   -> h2 ~a (List.map inside inline)
        | `three -> h3 ~a (List.map inside inline)
        | `four  -> h4 ~a (List.map inside inline)
        end
      | Code_bloc (options, inside) -> pre (List.map inside inline)
      | Paragraph -> p []
      | Line_break -> br ()
      | Inline i ->  span (List.map i inline)
      | Numbered_list items ->
        ol (List.map items (function `item ls -> li (List.map ls structural)))
      | Unnumbered_list items ->
        ul (List.map items (function `item ls -> li (List.map ls structural)))
    in
    List.map document structural



  let make ~configuration =
    (fun path () ->
      let open Html5 in
      let content =
        begin match Configuration.root_path configuration with
        | Some rootpath ->
          read_file (rootpath ^/ "doc" ^/
                       (sprintf "%s.sdx" (String.concat path ~sep:"/")))
        | None ->
          error `root_directory_not_configured
        end
        >>= fun file_content ->
        begin match Sequme_doc_syntax.parse ~pedantic:false file_content with
        | Ok document ->
          let subdocs = Sequme_doc_syntax.extract_level_one document in
          let divs =
            List.map subdocs (fun (_, title, content) ->
              let sec1 = Sequme_doc_syntax.Section (`one, None, title) in
              let (toc, doc) =
                Sequme_doc_syntax.table_of_contents ~toplevel:`two content in
              let header = sdx_to_html [sec1] in
              let tochtml =
                sdx_to_html [Sequme_doc_syntax.toc_to_numbered_list toc] in
              let content_html = sdx_to_html doc in
              div [div ~a:[a_class ["doc_doc"]] header;
                   div ~a:[ a_class ["doc_toc"]] tochtml;
                   div ~a:[a_class ["doc_doc"]] content_html])
          in
          return divs
        | Error _ ->
          return [pcdata "Error: could not parse content"]
        end
      in
      Template.default ~title:"Home" content)
end
module File_service = struct

  let people_of_libraries ~layout libs =
    while_sequential libs ~f:(fun sl ->
      layout#input_library#all
      >>| List.filter ~f:(fun il -> il#library#id = sl#g_id))
    >>| List.concat
    >>= while_sequential ~f:(fun il ->
      layout#lane#all
      >>| List.filter ~f:(fun lane ->
        Array.exists lane#libraries (fun l -> l#id = il#g_id)))
    >>| List.concat
    >>| List.map ~f:(fun lane ->
      Array.(to_list (map lane#contacts ~f:(fun c -> c#pointer))))
    >>| List.concat
    
  let indentify_and_verify ~configuration vol path =
    with_database ~configuration (fun ~dbh ->
      let vol_pointer = Layout.File_system.(unsafe_cast vol) in
      Common.all_paths_of_volume ~dbh ~configuration vol_pointer
      >>= fun all_paths ->
      let layout = Classy.make dbh in
      layout#file_system#get vol_pointer
      >>= fun vol_content ->
      begin match vol_content#g_kind with
      | `protocol_directory ->
        layout#protocol#all
        >>| List.filter ~f:(fun p -> p#doc#id = vol_content#g_id)
        >>= while_sequential ~f:(fun prot ->
          layout#stock_library#all
          >>| List.filter ~f:(fun sl ->
            match sl#protocol with None -> false | Some p -> p#id = prot#g_id))
        >>| List.concat
      | `bioanalyzer_directory  ->
        layout#bioanalyzer#all
        >>| List.filter ~f:(fun p ->
          match p#files with None -> false | Some f -> f#id = vol_content#g_id)
        >>= while_sequential ~f:(fun bio -> bio#library#get)
      | `agarose_gel_directory ->
        layout#agarose_gel#all
        >>| List.filter ~f:(fun p ->
          match p#files with None -> false | Some f -> f#id = vol_content#g_id)
        >>= while_sequential ~f:(fun ag -> ag#library#get)
      | k -> error (`forbidden_volume_kind k)
      end
      >>= fun stock_libraries ->
      people_of_libraries ~layout stock_libraries
      >>= fun people ->
      Authentication.authorizes (`view (`libraries_of people))
      >>= begin function
      | true -> return ()
      | false -> error (`wrong_access_rights)
      end
      >>= fun () ->
      let content_type =
        let mime_assoc = Ocsigen_charset_mime.default_mime_assoc () in
        Ocsigen_charset_mime.find_mime path mime_assoc in
      begin match
          List.mem all_paths path && Eliom_registration.File.check_file path
      with
      | true ->
        return (content_type, path)
      | false -> error (`path_not_right_volume path)
      end)
    
  let error_content e =
    let open Template in
    let open Html5 in
    return ( [Html5.pcdata "Error: Cannot retrieve that file …"])
      
  let make ~configuration =
    begin fun (vol, path) () ->
      let open Lwt  in
      indentify_and_verify ~configuration vol path
      >>= begin function
      | Ok (content_type, path) ->
        Eliom_registration.File.send ~content_type path
      | Error e ->
          (* Lwt.fail Eliom_common.Eliom_404 *)
        Template.default ~title:"File Error" (error_content e)
        >>= fun html ->
        Eliom_registration.Html5.send ~content_type:"text/html" html
      end
    end
      
end

let () =
  let open Lwt in
  Ocsigen_extensions.register_command_function ~prefix:"maintenance"
    (fun s c -> match c with
    | ["on"] ->
      log "Goin' maintenance mode\n" >>= fun _ ->
      Authentication.maintenance_mode_on ();
      return ()
    | ["off"] ->
      log "Leavin' maintenance mode\n" >>= fun _ ->
      Authentication.maintenance_mode_off ();
      return ()
    | _ -> raise Ocsigen_extensions.Unknown_command)

let () =
  Eliom_service.register_eliom_module
    "hitscoreweb"
    (fun () ->

      Lwt_preemptive.init 1 500 (eprintf "LwtP:%s\n%!");

      Sequme_flow_sys.Timeout.set_global_default 10.;
      
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
          Lwt.bind (Template.default (error e))
            (Eliom_registration.Html5.send ?code) in
        Eliom_registration.set_exn_handler (function
          | Eliom_common.Eliom_404 -> send ~code:404 `eliom_404
          | Eliom_common.Eliom_Wrong_parameter -> send `eliom_wrong_parameter
          | Eliom_common.Eliom_Typing_Error l -> send (`eliom_typing_error l)
          (*
TODO: All exceptions in coservices should be handled in some other way
          *)
          | e ->
            eprintf "EXN: %s\n%s\n%!" (Exn.to_string e)
              (Exn.backtrace ()); raise e)
      in

      let state, hitscore_configuration, debug_mode =

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
            Some (Configuration.db_configuration
                    ~host ~port ~database ~username ~password)
          | _ -> None
        in
        let config =
          Configuration.configure
            ?vol_directory:!vols ?raw_data_path:!raw ?hiseq_directory:!hsd
            ?root_path:!rodi ?db_configuration () in
        Authentication.init ~disabled:!debug_mode ?pam_service:!pam_service config;
        Web_data_access.init
          ~loop_time:(if !debug_mode then 90. else 600.)
          ~configuration:config ()
        |! Lwt.ignore_result;
        let state = 
          Hitscoreweb_state.init_state ~configuration:config () in
        (state, config, debug_mode)
      in

      Services.(register default) (Default_service.make ~state);
      Services.(register home) (Default_service.make ~state);
      Services.(register test) (Test_service.make ~state);

      Services.(register hiseq_runs)
        Hitscoreweb_hiseq_runs.(make hitscore_configuration);

      Services.(register facility_statistics)
        Hitscoreweb_facility_stats.(make ~state);

      Services.(register persons) Persons_service.(make ~state);

      Services.(register libraries)
        Hitscoreweb_libraries.(
          make
            ~information_cache_timming:(
              if !debug_mode then (40., 60.) else (60., 1200.))
            ~configuration:hitscore_configuration);

      Services.(register flowcell)
        Flowcell_service.(make hitscore_configuration);

      Services.(register evaluations)
        Evaluations_service.(make ~configuration:hitscore_configuration);

      Layout_service.init_caml_service
        ~configuration:hitscore_configuration ();
      Services.(register layout)
        Layout_service.(make ~configuration:hitscore_configuration);

      Services.(register doc)
        Doc_service.(make ~configuration:hitscore_configuration);

      One_person_service.init_caml_service ~state ();
      One_person_service.init_email_verification_service ~state;
      Services.(register self) One_person_service.(make_self ~state);

      Services.(register person) One_person_service.(make_person ~state);

      Services.(register log) Hitscoreweb_log.(make ~state);

      Services.(register uploads) Hitscoreweb_uploads_service.(make ~state);

      Services.(register_css stylesheet)
        Template.(css_service_handler ~configuration:hitscore_configuration);

      Services.(register_file file)
        File_service.(make ~configuration:hitscore_configuration);

      logf "All services are registered" |! Lwt.ignore_result;

    )

