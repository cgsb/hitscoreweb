open Hitscoreweb_std


let make f = 
  let content = ref None in
  fun () ->
    match !content with
    | None -> 
      let s = f () in          
      content := Some s;
      s
    | Some s -> s

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

let login =
  make (Eliom_services.service
          ~path:["login"]
          ~get_params: Eliom_parameters.unit)
  
let logout =
  make (Eliom_services.service
          ~path:["logout"]
          ~get_params:Eliom_parameters.unit)
          
let layout =
  make (Eliom_services.service ~path:["layout"]
          ~get_params:Eliom_parameters.(set string "type" ** set int "value"))

let link service =
  Eliom_output.Html5.a ~service:(service ())


let register f =
  Eliom_output.Html5.register 
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
