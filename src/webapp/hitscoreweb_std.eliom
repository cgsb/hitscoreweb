include Core.Std
(* include Lwt *)

let (|>) x f = f x
{shared{
module Html5 = struct
  include Eliom_content.Html5.D
  open Printf 
  let pcdataf fmt = ksprintf pcdata fmt

  let codef fmt = ksprintf (fun s -> code [pcdata s]) fmt

  let strongf fmt = ksprintf (fun s -> strong [pcdata s]) fmt

  let a_hreff fmt = ksprintf (fun s -> a_href (Xml.uri_of_string s)) fmt

  let core_a = Eliom_content_core.Html5.D.a

end
}}
{client{
module Html5_to_dom = Eliom_content.Html5.To_dom
}}
module Tyxml = Xml
module Output_app =
  Eliom_registration.App (struct
    let application_name = "hitscoreweb"
  end)

module PGOCaml = struct end

include Hitscore
include Sequme_flow
include Sequme_flow_list
include Sequme_flow_sys

module Xml_tree = struct
  include Biocaml_internal_xmlm
  let in_tree i = 
    let el tag childs = `E (tag, childs)  in
    let data d = `D d in
    input_doc_tree ~el ~data i
end

  
let log_session_info =
  Eliom_reference.eref ~scope:Eliom_common.session "NO-SESSION"

let log_function  =
  ref (None : (string -> unit Lwt.t) option)

let log s =
  begin match !log_function with
  | None -> 
    let open Lwt in
    let file_name =
      Filename.(
        concat
          (dirname (Ocsigen_messages.error_log_path ())) "hitscoreweb.log") in
    (* eprintf "logging to %s\n%!" file_name; *)
    Lwt_log.file ~mode:`Append
      ~template:"$(message)"
      ~file_name ()
    >>= fun file_logger ->
    let lwt_log s = Lwt_log.notice ~logger:file_logger s in
    log_function := Some lwt_log;
    return (Ok lwt_log)
  | Some f -> return f 
  end
  >>= fun lwt_log ->
  begin
    try (* Hackish way of knowing is session info is available. *)
      if Eliom_state.(
        volatile_data_state_status ~scope:Eliom_common.session ()
        = Alive_state)
      then
        wrap_io Eliom_reference.get log_session_info
      else 
        return "NO-SESSION"
    with
      e -> return "NO-SESSION"
  end
  >>= fun session_info ->
  wrap_io lwt_log (sprintf "[%s][%s] %s" Time.(now () |! to_string) session_info s)

let logf fmt = ksprintf log fmt

  
(*
  https://bitbucket.org/yminsky/ocaml-core/src/c0e9df7b574d/base/core/extended/lib/sendmail.mli
*)
let send_mail ?subject ?sender ?cc ?bcc ?reply_to ?content_type ~recipients content =
  let m =
    wrap_io
      (Lwt_preemptive.detach
         (Core_extended.Sendmail.send ?subject ?sender ?cc ?bcc ?reply_to
            ?content_type ~recipients))
      content
  in
  double_bind m ~ok:return ~error:(function
  | `io_exn e -> error (`sendmail e))
    
let rec interleave_list ~sep = function
  | [] -> []
  | [one] -> [one]
  | l :: t -> l :: sep :: interleave_list ~sep t

let array_to_list_intermap ~sep ~f a =
  interleave_list ~sep (List.map (Array.to_list a) ~f)

let rec interleave_map ~sep ~f = function
  | [] -> []
  | [one] -> [f one]
  | h :: t -> (f h) :: sep :: (interleave_map ~sep ~f t)

let layout_log ~dbh fmt =
  ksprintf (Common.add_log ~dbh) fmt

(*
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
*)

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

module String = struct
  include String
end

let dbg fmt =
  Printf.ksprintf (fun s -> Firebug.console##debug(Js.string s)) fmt
  
}}

let pretty_string_of_float ?(sof=sprintf "%.3f") f = 
  let s = sof f in
  let rec f s =
    if String.(length s) > 3 then
      String.(f (drop_suffix s 3) ^ "," ^ suffix s 3)
    else
      s in
  let prefix s =
    let length = max 0 (18 - String.(length s)) in
    sprintf "%s%s"
      (String.concat ~sep:"" (List.init length (fun _ -> " "))) s
  in
  match String.split s ~on:'.' with
  | [] | [_] -> prefix (f s)
  | one :: more ->
    sprintf "%s.%s" (prefix (f one)) (String.concat ~sep:"" more)

let make_delayed f = 
  let content = ref None in
  fun () ->
    match !content with
    | None -> 
      let s = f () in          
      content := Some s;
      s
    | Some s -> s
      

let unique_id: string -> string =
  let i = ref 0 in
  (fun s -> incr i; sprintf "%s_%d" s !i)


(* ********************************************************************** *)
{client{

(* This is still not perfect as it goes to the top of the page *)
let reload () =
  Eliom_client.change_page ~service: Eliom_service.void_coservice' () ()

}}

