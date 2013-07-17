(* include Core.Std *)
(* include Lwt *)

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
module Html5_manip = Eliom_content.Html5.Manip
module Html5_set_css = Eliom_content.Html5.Manip.SetCss

(* Redefine the Array module to make _type.mli files happy *)
module Array = struct type 'a t = 'a array include Array end
module List = ListLabels
module String = struct
  include StringLabels
end

let (|>) x f = f x
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


let dbg fmt =
  Printf.ksprintf (fun s -> Firebug.console##debug(Js.string ("DBG: " ^ s))) fmt

}}

(* ********************************************************************** *)
{client{

(* This is still not perfect as it goes to the top of the page *)
let reload () =
  Eliom_client.change_page ~service: Eliom_service.void_coservice' () ()


let get_current_host_port () =
  let open Url in
  begin match Current.get () with
  | Some (Https { hu_host; hu_port; hu_arguments })
  | Some (Http { hu_host; hu_port; hu_arguments }) ->
    (* dbg "Args:"; *)
    (* List.iter  hu_arguments (fun (a,b) -> dbg " * %s %s" a  b); *)
    (hu_host, hu_port)
  | Some u ->
    dbg "URL: %s" (Url.string_of_url u); failwith "no URL"
  | None -> dbg "NO URL" ; failwith "no URL"
  end
}}


let processing_onclick_handler ~id_to_hide ~message_span_id =
  Html5.a_onclick {{fun _ ->
      let form_span =
        Dom_html.document##getElementById (Js.string %id_to_hide) in
      let message_span =
        Dom_html.document##getElementById (Js.string %message_span_id) in
      Js.Opt.iter form_span (fun span ->
          span##style##visibility  <- Js.string "hidden";);
      Js.Opt.iter message_span (fun span ->
          span##style##visibility  <- Js.string "visible";
          span##innerHTML <- Js.string "<b>Processing …</b>";);
    }}

(*
  Take a div and make it scroll to the bottom, while loading the page.

  See for example the /log service, displaying the end of the logs.
*)
let make_div_scrolled_to_bottom box =
  ignore {unit{
    let elt = Html5_to_dom.of_div %box in
    elt##scrollTop <- elt##scrollHeight;
  }};
  ()
