include Core.Std
(* include Lwt *)

let (|>) x f = f x

module Html5 = struct
  include Eliom_pervasives.HTML5.M
    
  let pcdataf fmt = ksprintf pcdata fmt

  let codef fmt = ksprintf (fun s -> code [pcdata s]) fmt

  let a_hreff fmt = ksprintf (fun s -> a_href (XML.uri_of_string s)) fmt


end
module Output_app =
  Eliom_output.Eliom_appl (struct
    let application_name = "hitscoreweb"
  end)

module Lwt_config = struct
  include Lwt
  include Lwt_chan
  let map_sequential l ~f = Lwt_list.map_s f l
  let log_error s = output_string Lwt_io.stderr s >>= fun () -> flush Lwt_io.stderr
    
  exception System_command_error of Lwt_unix.process_status
  let system_command s = 
    Lwt_unix.(
      system s >>= function
      | WEXITED 0 -> return ()
      | e -> fail (System_command_error e))

  let write_string_to_file s f =
    Lwt_io.(
      with_file ~mode:output f (fun o ->
        output_string o s))

end
module Hitscore_lwt = Hitscore.Make(Lwt_config)
module Layout = Hitscore_lwt.Layout
module PGOCaml = Layout.PGOCaml
module Configuration = Hitscore_lwt.Configuration

include Hitscore_lwt.Result_IO

module Xml_tree = struct
  include Xmlm
  let in_tree i = 
    let el tag childs = `E (tag, childs)  in
    let data d = `D d in
    input_doc_tree ~el ~data i
end
let read_file file =
  wrap_io Lwt_io.(fun () -> with_file ~mode:input file (fun i -> read i)) ()

    
let rec interleave_list ~sep = function
  | [] -> []
  | [one] -> [one]
  | l :: t -> l :: sep :: interleave_list ~sep t

let array_to_list_intermap ~sep ~f a =
  interleave_list ~sep (List.map (Array.to_list a) ~f)


let layout_log ~dbh fmt =
  let f log =
    Layout.Record_log.add_value ~dbh ~log >>= fun _ -> return () in
  ksprintf f fmt

let pg_raw_query ?with_log ~dbh ~query =
  let module PG = Layout.PGOCaml in
  let name = "todo_change_this" in
  let execution = 
    wrap_io (PG.prepare ~name ~query dbh) ()
    >>= fun () ->
    wrap_io (PG.execute ~name ~params:[] dbh) ()
    >>= fun result ->
    wrap_io (PG.close_statement dbh ~name) ()
    >>= fun () ->
    return result
  in
  match with_log with
  | None -> execution
  | Some tag ->
    double_bind execution
      ~ok:(fun x -> 
        layout_log ~dbh "(%s success %S)" tag query
        >>= fun () ->
        return x)
      ~error:(function
      | `io_exn e as err ->
        layout_log ~dbh "(%s error %S %S)" tag query (Exn.to_string e)
        >>= fun () -> 
        error err
      | err -> error err)

{client{
let (|!) x f = f x
let lwtunit (x: unit Lwt.t) = Pervasives.ignore x
let ($) f x = f x
  
let float_of_string s =
  (* imitate ocaml's runtime: *)
  let f = float_of_string s in
  match classify_float f with
  | FP_infinite | FP_nan -> failwith "float_of_string"
  | _ -> f
    
let get_element_exn s =
  Js.Opt.get (Dom_html.document##getElementById (Js.string s))
    (fun _ -> Printf.ksprintf failwith "Getting %S -> ERROR " s)
  
let get_element s =
  Js.Opt.to_option (Dom_html.document##getElementById (Js.string s))

}}


let make_delayed f = 
  let content = ref None in
  fun () ->
    match !content with
    | None -> 
      let s = f () in          
      content := Some s;
      s
    | Some s -> s
      

let unique_id =
  let i = ref 0 in
  (fun s -> incr i; sprintf "%s_%d" s !i)

  
(* ********************************************************************** *)
(* Debug service *)
{shared{
  type debug_message = string deriving (Json)
}}
let debug_service =
  make_delayed (Eliom_services.service 
          ~path:["debug service"]
          ~get_params:Eliom_parameters.(caml "param" Json.t<debug_message>))

let debug_messages = ref []

let init_debug () =
  Eliom_output.Caml.register ~service:(debug_service ())
    (fun param () ->
      debug_messages := (Time.now (), param) :: !debug_messages;
      Lwt.return ())
{client{
let debugf service fmt =
  Printf.ksprintf
    (fun msg -> ignore (Eliom_client.call_caml_service ~service msg ()))
    fmt
}}

(* ********************************************************************** *)
{client{

(* This is still not perfect as it goes to the top of the page *)
let reload () =
  lwtunit $ Eliom_client.change_page
    ~service: Eliom_services.void_coservice' () ()

}}

