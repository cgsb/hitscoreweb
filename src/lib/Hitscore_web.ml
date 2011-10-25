open Hitscore_web_std




let default () =
  let default =
    Eliom_services.service
      ~path:[""] ~get_params:Eliom_parameters.unit () in
  Eliom_output.Html5.register ~service:default
    (fun () () ->
      eprintf "Registering default\n%!";
      Lwt.return
        (let open HTML5.M in
         html
           (head (title (pcdata "Hitscore Web")) [])
           (body [p [pcdata "Histcore default web page."]])))

let () =

  Eliom_services.register_eliom_module
    "hitscoreweb" 
    (fun () ->
      default ()
    )

