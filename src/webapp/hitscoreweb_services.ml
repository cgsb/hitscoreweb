open Hitscoreweb_std_server

let make = make_delayed

let default =
  make (Eliom_service.service ~path:[""] ~get_params:Eliom_parameter.unit)
let home =
  make (Eliom_service.service ~path:["home"] ~get_params:Eliom_parameter.unit)

let hiseq_runs =
  make
    (Eliom_service.service ~path:["hiseq_runs"] ~get_params:Eliom_parameter.unit)

let pgm_runs =
  make
    (Eliom_service.service ~path:["pgm_runs"] ~get_params:Eliom_parameter.unit)

let facility_statistics =
  make
    (Eliom_service.service ~path:["stats"] ~get_params:Eliom_parameter.unit)

let log =
  make
    (Eliom_service.service ~path:["log"] ~get_params:Eliom_parameter.unit)

let flowcell =
  make
    (Eliom_service.service ~path:["flowcell"]
       ~get_params:Eliom_parameter.(string "serial"))

let persons =
  make (
    Eliom_service.service
      ~path:["persons"]
      ~get_params:Eliom_parameter.(opt (bool "transpose")
                                    ** set string "email"))

let uploads =
  make
    (Eliom_service.service ~path:["uploads"] ~get_params:Eliom_parameter.unit)

let test =
  make (
    Eliom_service.service ~path:["test"] ~get_params:Eliom_parameter.(unit))

let submission_forms =
  make (
    Eliom_service.service ~path:["submissions"]
      ~get_params:Eliom_parameter.(unit))

let fastx_results =
  make (
    Eliom_service.service
      ~path:["fastx_results"]
      ~get_params:Eliom_parameter.(opt (string "path")))

type libraries_show = [ `basic | `stock | `fastq | `details ]
let libraries_show_of_string = function
  | "basic" -> `basic
  | "stock" -> `stock
  | "fastq" -> `fastq
  | "details" -> `details
  | s -> failwith "libraries_show_of_string"
let string_of_libraries_show  = function
  | `basic -> "basic"
  | `stock -> "stock"
  | `fastq -> "fastq"
  | `details -> "details"
let libraries_show_eliom_type =
  Eliom_parameter.user_type
    ~of_string:libraries_show_of_string
    ~to_string:string_of_libraries_show

let libraries =
  make (
    Eliom_service.service
      ~path:["libraries"]
      ~get_params:Eliom_parameter.(set libraries_show_eliom_type "show"
                                    ** set string "qualified_name"))

let evaluations =
  make (Eliom_service.service
          ~path:["evaluations"]
          ~get_params: Eliom_parameter.unit)

let layout =
  make (Eliom_service.service ~path:["layout"]
          ~get_params:Eliom_parameter.(set string "type" ** set int "value"))

let stylesheet =
  make (Eliom_service.service
          ~path:["gencore_stylesheet"]
          ~get_params: Eliom_parameter.unit)

let doc =
  make (Eliom_service.service
  ~path:["doc"]
  ~get_params:Eliom_parameter.(suffix (all_suffix "path")))


let self =
  make (Eliom_service.service
          ~path:["self"]
          ~get_params:Eliom_parameter.(opt (string "action")))
let person =
  make (Eliom_service.service
          ~path:["person"]
          ~get_params:Eliom_parameter.(string "id" ** opt (string "action")))

let file =
  make (Eliom_service.service
          ~path:["file"]
          ~get_params:Eliom_parameter.(suffix (int "vol" ** string "path")))
let register_file s =
  Eliom_registration.Any.register ~service:(s ())

let register f =
  Output_app.register
    ~content_type:"text/html"
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
  Eliom_registration.CssText.register
    ~service:(f ())
