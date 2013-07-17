
open Hitscoreweb_std_server
module Template = Hitscoreweb_template
module Authentication = Hitscoreweb_authentication

let make ~state =
  (fun () () ->
    let open Html5 in
    let configuration = state.Hitscoreweb_state.configuration in
    Template.default ~title:"PGM Runs"
      (Authentication.authorizes (`view `all_pgm_runs)
       >>= begin function
       | true ->
         Template.(make_content ~configuration
                     ~main_title:"PGM Runs" (content_list [] |> return))
       | false ->
         Template.make_authentication_error ~configuration
           ~main_title:"PGM Runs"
           (return [Html5.pcdataf "You may not view anything here."])
       end))
