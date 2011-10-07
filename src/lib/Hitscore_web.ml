open Hitscore_web_std




let default = Eliom_output.Html5.register_service [""] Eliom_parameters.unit
  (fun () () ->
    Lwt.return
     (let open HTML5.M in
      html
        (head (title (pcdata "Hitscore Web")) [])
        (body [p [pcdata "Histcore default web page."]])))
  

