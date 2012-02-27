{shared{
open Hitscoreweb_std
}}

let make = make_delayed

let default =
  make (Eliom_services.service ~path:[""] ~get_params:Eliom_parameters.unit)
let home =
  make (Eliom_services.service ~path:["home"] ~get_params:Eliom_parameters.unit)

let flowcells =
  make
    (Eliom_services.service ~path:["flowcells"] ~get_params:Eliom_parameters.unit)

let flowcell =
  make
    (Eliom_services.service ~path:["flowcell"]
       ~get_params:Eliom_parameters.(string "serial"))

let persons =
  make (
    Eliom_services.service
      ~path:["persons"] 
      ~get_params:Eliom_parameters.(opt (bool "transpose")
                                    ** set string "email"))

let libraries =
  make (
    Eliom_services.service
      ~path:["libraries"] 
      ~get_params:Eliom_parameters.(opt (bool "transpose")
                                    ** set string "qualified_name"))

let evaluations =
  make (Eliom_services.service
          ~path:["evaluations"]
          ~get_params: Eliom_parameters.unit)
          
let layout =
  make (Eliom_services.service ~path:["layout"]
          ~get_params:Eliom_parameters.(string "action" 
                                        ** set string "type" ** set int "value"))

let stylesheet =
  make (Eliom_services.service
          ~path:["gencore_stylesheet"]
          ~get_params: Eliom_parameters.unit)
    
let link service =
  Eliom_output.Html5.a ~service:(service ())


let register f =
  Output_app.register 
    ~error_handler:(fun sel -> 
      List.iter sel ~f:(fun (s, e) -> 
        eprintf "Errors: %S %S\n%!" s (Exn.to_string e));
      Lwt.return 
        Html5.(html
                 (head (title (ksprintf pcdata "Hitscoreweb: ERROR")) [])
                 (body [
                   div [
                     ksprintf pcdata "ERROR:";
                     ul (List.map sel (fun (s, e) -> 
                       li [ksprintf pcdata "%S: %S" s (Exn.to_string e)]));
                   ];
                 ])))
    ~service:(f ())


let register_css f =
  Eliom_output.CssText.register 
    ~service:(f ())
