
open Hitscoreweb_std_server
module Template = Hitscoreweb_template
module Services = Hitscoreweb_services
module Authentication = Hitscoreweb_authentication
module Msg = Hitscoreweb_messages
module Web_data_access = Hitscoreweb_data_access

let all_pgm_runs ~configuration =
  let open Html5 in
  let qname stock =
    match stock#project with
    | None -> stock#name
    | Some p -> sprintf "%s.%s" p stock#name in
  let start = Time.(now () |> to_float) in
  Web_data_access.classy_cache ()
  >>= fun cache ->
  let all_persons = cache#persons in
  let all_pgm_runs = cache#pgm_runs in
  let all_pgm_pools = cache#pgm_pools in
  let all_invoicings = cache#invoicings in
  let all_pgm_stock_libs = cache#pgm_stock_libs in
  let after_db = Time.(now () |> to_float) in
  let pgm_run_rows =
    List.map all_pgm_runs (fun pgm_run ->
        let title_cell =
          `sortable (Time.to_string pgm_run#date,
                     [strong [
                         pcdata pgm_run#sequencer; pcdata ": "; br ();
                         pcdata (Time.to_local_date pgm_run#date
                                 |> Date.to_string)];
                      br ();
                      pcdataf " (%s bp – %s Chip)"
                        pgm_run#run_type pgm_run#chip_type;
                     ]) in
        let pools =
          let libraries_of_pool pp =
            let qnames = ref [] in
            let list_of_links =
              List.filter_map all_pgm_stock_libs (fun (il, sl) ->
                  if Array.exists pp#libraries (fun l -> l#id = il#g_id)
                  then begin
                    let qn = qname sl in
                    qnames := qn :: !qnames;
                    Some (span [
                        Template.a_link Services.libraries
                          [pcdata sl#name] ([`basic;`fastq], [qn]);
                      ])
                  end
                  else None)
              |> interleave_list ~sep:(pcdata ", ")
            in
            list_of_links @ [
              pcdata " (";
              Template.a_link Services.libraries [pcdata "all"]
                ([`basic; `fastq], !qnames);
              pcdata ")"
            ]
          in
          let contacts_of_pool pool =
            List.filter_map all_persons (fun p ->
                if Array.exists pool#contacts (fun c -> c#id = p#g_id)
                then Some (span [
                    Template.a_link Services.persons
                      [pcdata p#family_name] (Some true, [p#email])
                  ])
                else None)
            |> interleave_list ~sep:(pcdata ", ")
          in
          let invoices_of_pool pool =
            let pi_link id =
              List.find_map all_persons (fun p ->
                  if p#g_id = id
                  then Some (
                      Template.a_link Services.persons
                        [pcdata p#family_name] (Some true, [p#email]))
                  else None)
              |> Option.value
                ~default:(Template.error_span [pcdataf "PERSON %d ???" id])
            in
            List.filter_map all_invoicings (fun i ->
                if Array.exists pool#invoices (fun c -> c#id = i#g_id)
                then Some (span [
                    pi_link i#pi#id; pcdataf ": %.0f %%" i#percentage;
                    Option.value_map i#note ~default:(span [])
                      ~f:(fun s -> pcdataf " (%s)" s);
                  ])
                else None)
            |> interleave_list ~sep:(pcdata ", ")
          in
          List.filter_map all_pgm_pools (fun pp ->
              if Array.exists pgm_run#pool (fun p -> p#id = pp#g_id)
              then
                let name = Option.value ~default:"NO_NAME" pp#pool_name in
                Some [
                  `sortable (name, [pcdataf "Pool %s" name]);
                  `text [span (libraries_of_pool pp)];
                  `text [span (contacts_of_pool pp)];
                  `text [span (invoices_of_pool pp)];
                ]
              else None)
          |> (fun cells -> `subtable cells)
        in
        [title_cell; pools])
  in
  let header =
    [`head_cell Msg.pgm_run_title;
     `head_cell Msg.pgm_run_pools;
     `head_cell Msg.pgm_run_libraries;
     `head_cell Msg.pgm_run_contacts;
     `head_cell Msg.pgm_run_invoicing;
    ] in
  let after_table = Time.(now () |> to_float) in
  return Template.(content_list [
      content_section (pcdata "Benchmarks")
        (content_paragraph [
            ul [
              li [pcdataf "DB accesses: %f s." (after_db -. start)];
              li [pcdataf "Formatting: %f s." (after_table -. after_db)];
            ]
          ]);
      content_section
        (pcdataf "The %d PGM Runs" (List.length all_pgm_runs))
        (content_table (header :: pgm_run_rows))
    ])

let make ~state =
  (fun () () ->
    let open Html5 in
    let configuration = state.Hitscoreweb_state.configuration in
    Template.default ~title:"PGM Runs"
      (Authentication.authorizes (`view `all_pgm_runs)
       >>= begin function
       | true ->
         Template.(make_content ~configuration
                     ~main_title:"PGM Runs" (all_pgm_runs ~configuration))
       | false ->
         Template.make_authentication_error ~configuration
           ~main_title:"PGM Runs"
           (return [Html5.pcdataf "You may not view anything here."])
       end))
