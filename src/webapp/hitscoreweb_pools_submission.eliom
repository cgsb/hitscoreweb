{shared{
open Hitscoreweb_std
}}

module Queries = Hitscoreweb_queries

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template


let start ~configuration ~main_title =
  (error (`not_implemented "pools-submission"))
  
let make ~configuration =
  (fun () () ->
    let main_title = "Submit Pools" in
    Template.default ~title:main_title
      (Authentication.authorizes (`perform `pools_submission)
       >>= function
       | true -> start ~configuration ~main_title
       | false ->
         Template.make_authentication_error ~configuration ~main_title
           (return [Html5.pcdataf "You may not submit new libraries."])))
