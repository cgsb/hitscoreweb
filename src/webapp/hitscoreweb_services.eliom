{shared{
open Hitscoreweb_std
}}

let make = make_delayed

let default =
  make (Eliom_services.service ~path:[""] ~get_params:Eliom_parameters.unit)
let home =
  make (Eliom_services.service ~path:["home"] ~get_params:Eliom_parameters.unit)

let hiseq_runs =
  make
    (Eliom_services.service ~path:["hiseq_runs"] ~get_params:Eliom_parameters.unit)

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

type libraries_show = [ `basic | `stock | `fastq ]
let libraries_show_of_string = function
  | "basic" -> `basic
  | "stock" -> `stock
  | "fastq" -> `fastq
  | s -> failwith "libraries_show_of_string"
let string_of_libraries_show  = function
  | `basic -> "basic"
  | `stock -> "stock"
  | `fastq -> "fastq"
let libraries_show_eliom_type =
  Eliom_parameters.user_type
    ~of_string:libraries_show_of_string
    ~to_string:string_of_libraries_show
  
let libraries =
  make (
    Eliom_services.service
      ~path:["libraries"] 
      ~get_params:Eliom_parameters.(set libraries_show_eliom_type "show"
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
    
let doc =
  make (Eliom_services.service
  ~path:["doc"]
  ~get_params:Eliom_parameters.(suffix (all_suffix "path")))


let self =
  make (Eliom_services.service
          ~path:["self"]
          ~get_params:Eliom_parameters.(opt (string "action")))
let person =
  make (Eliom_services.service
          ~path:["person"]
          ~get_params:Eliom_parameters.(string "id" ** opt (string "action")))
  
    
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
