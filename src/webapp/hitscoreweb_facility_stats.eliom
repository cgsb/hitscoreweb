
open Hitscoreweb_std

module Services = Hitscoreweb_services
module Authentication = Hitscoreweb_authentication
module Template = Hitscoreweb_template
module Persons_service = Hitscoreweb_persons

let gencore_users_stats layout =
  let open Template in
  let open Html5 in
  layout#lane#all >>= fun lanes ->
  layout#person#all
  >>| List.filter ~f:(fun p -> Array.exists p#roles ((=) `pi))
  >>= while_sequential ~f:(fun pi ->
    while_sequential (Array.to_list pi#affiliations) (fun aff_p ->
      aff_p#get)
    >>= fun affs ->
    let their_lanes =
      List.filter lanes (fun l ->
        Array.exists l#contacts (fun c -> c#id = pi#g_id)) in
    return (object
      method pi = pi
      method affiliations = affs
      method nb_of_lanes = List.length their_lanes
      method nb_of_libraries =
        List.fold_left their_lanes ~init:0 ~f:(fun s l ->
          s + (Array.length l#libraries))
    end))
  >>= fun informed_pis ->
  let table filter =
    let concerned =
      List.filter informed_pis (fun p ->
        List.exists p#affiliations (fun a -> filter a#path)) in
    let rows =
      List.map concerned (fun pi ->
        [ `text [pcdataf "%s" pi#pi#family_name];
          `text [pcdataf "%d" pi#nb_of_libraries];
          `text [pcdataf "%d" pi#nb_of_lanes]; ]) in
    content_table (
      [`head [pcdata "Name"]; `head [pcdata "# Libraries"]; `head [pcdata "# Lanes"]]
      :: rows
    )
  in
  return (content_section (pcdata "Gencore User Scores")
            (content_list [
              content_section (pcdata "CGSB")
                (table ((=) [| "NYU"; "Biology"; "CGSB" |]));
              content_section (pcdata "Bio. Dept.")
                (table ((=) [| "NYU"; "Biology" |]));
              content_section (pcdata "The Others")
                (table (fun a ->
                  a <> [| "NYU"; "Biology" |]
                  && a <> [| "NYU"; "Biology"; "CGSB" |]));
            ]))


let flowcell_data layout =
  layout#hiseq_run#all
  >>| List.sort ~cmp:(fun a b -> compare a#date b#date)
  >>= while_sequential ~f:(fun hsr ->
    let l = [hsr#flowcell_a; hsr#flowcell_b] |! List.filter_opt in
    while_sequential l (fun fc_p ->
      fc_p#get >>= fun fc ->
      while_sequential (Array.to_list fc#lanes) (fun l ->
        l#get >>= fun one_lane ->
        while_sequential (Array.to_list one_lane#contacts) (fun p ->
          p#get >>= fun person ->
          if Array.exists person#roles ((=) `pi)
          then return  (Some person)
          else return None)
        >>| List.filter_opt
        >>= fun pi_s ->
        let run_type =
          match one_lane#requested_read_length_1, one_lane#requested_read_length_2 with
          | n, None -> sprintf "SE %d" n
          | n, Some s -> sprintf "PE %dx%d" n s
        in
        return (pi_s, run_type, Array.length one_lane#libraries))
      >>= fun pi_runs ->
      return (object
        method run = hsr
        method fcid = fc#serial_name
        method run_type =
          List.nth pi_runs 1 |! Option.value_map ~default:"???" ~f:snd3
        method pi_s =
          List.map pi_runs (fun (pis, _, _) -> List.map pis (fun p -> p#family_name))
          |! List.concat
          |! List.dedup
        method nb_of_libraries = 
          List.fold_left (List.map pi_runs trd3)
            ~init:0 ~f:(fun a b -> a + b)
      end)))
  >>| List.concat

let mini_run_plan flowcells =
  let open Template in
  let open Html5 in
  let table =
    let rows =
      List.map flowcells (fun f ->
        [ `text [pcdata (Time.to_local_date f#run#date |! Date.to_string)];
          `text [pcdata f#fcid];
          `text [pcdata f#run_type];
          `text [pcdata String.(concat ~sep:", " f#pi_s)] ]) in
    content_table (
      [`head [pcdata "Run Date"]; `head [pcdata "FCID"];
       `head [pcdata "Run Type"]; `head [pcdata "P.I.(s)"] ]
      :: rows) in
  (content_section (pcdata "Mini-Run-Plan") table)
  

let libraries_per_month flowcells =
  let open Template in
  let open Html5 in
  let ym t =
    let d = Time.to_local_date t in
    (Date.year d, Date.month d) in
  let per_month =
    List.group flowcells ~break:(fun fa fb ->
      ym fa#run#date <> ym fb#run#date) in
  let table =
    let rows =
      List.map per_month (fun l ->
        let yr, mth = ym ((List.hd_exn l) #run#date) in
        let libraries =
          List.fold_left l ~init:0 ~f:(fun c f -> c + f#nb_of_libraries) in
        [ `text [pcdataf "%d, %s" yr (Month.to_string mth)];
          `text [pcdataf "%d" libraries] ]) in
    content_table ([ `head [pcdata "Month"]; `head [pcdata "# Libraries"] ] :: rows)
  in
  (content_section (pcdata "Libraries Per Month") table) 
    
let statistics_page configuration =
  let open Template in
  let open Html5 in
  with_database configuration (fun ~dbh ->
    let layout = Classy.make dbh in
    gencore_users_stats layout
    >>= fun users_section ->
    flowcell_data layout >>= fun flowcells ->
    return (content_list [users_section;
                          mini_run_plan flowcells;
                          libraries_per_month flowcells]))

let make ~configuration =
  (fun () () ->
    let main_title = "Facility Statistics" in
    Template.default ~title:main_title
      (Authentication.authorizes (`view `facility_statistics)
       >>= function
       | true ->
         Template.make_content ~configuration ~main_title    
           (statistics_page configuration);
       | false ->
         Template.make_authentication_error ~configuration ~main_title
           (return [Html5.pcdataf 
                       "You may not view the facility statistics."])))
