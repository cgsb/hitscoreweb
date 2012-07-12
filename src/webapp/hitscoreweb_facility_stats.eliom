
open Hitscoreweb_std

module Services = Hitscoreweb_services
module Authentication = Hitscoreweb_authentication
module Template = Hitscoreweb_template
module Persons_service = Hitscoreweb_persons

let ($) f x = f x

let statistics_page configuration =
  let open Template in
  let open Html5 in
  return $ content_paragraph []

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
