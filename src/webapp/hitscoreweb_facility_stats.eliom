
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
      [`head [pcdata "Name"]; `head [pcdata "# libraries"]; `head [pcdata "# Lanes"]]
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

  
let statistics_page configuration =
  let open Template in
  let open Html5 in
  with_database configuration (fun ~dbh ->
    let layout = Classy.make dbh in
    gencore_users_stats layout
    >>= fun users_section ->
    return (users_section))

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
