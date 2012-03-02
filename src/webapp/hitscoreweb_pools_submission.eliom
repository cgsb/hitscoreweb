{shared{
  
open Hitscoreweb_std
}}

module Queries = Hitscoreweb_queries

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

    
{shared{
type up_message =
| Request_project_list
deriving (Json)

type down_message = 
  | Project_list of string list
}}

let submission_caml_service =
  make_delayed (Eliom_services.service 
          ~path:["submission_caml_service"]
          ~get_params:Eliom_parameters.(caml "param" Json.t<up_message>))

let reply ~configuration =
  function
  | Request_project_list ->
    Hitscore_lwt.with_database ~configuration ~f:(fun ~dbh ->
      Layout.Record_stock_library.(
        get_all ~dbh
        >>= fun sls ->
        of_list_sequential sls ~f:(fun p ->
          get ~dbh p >>= fun {project} -> return project)
        >>= fun l -> 
        return (Project_list (List.filter_opt l |! List.dedup))))
      
let init_submission_caml_service ~configuration =
  let already = ref false in
  fun () ->
    if !already then () else (
      already := true;
      let fail fmt =
        ksprintf
          (fun s -> Lwt.fail (Failure ("wrong reply from server: " ^ s)))
          fmt
      in
      Eliom_output.Caml.register ~service:(submission_caml_service ())
        (fun param () ->
          Lwt.bind (reply ~configuration param) (function
          | Ok o -> Lwt.return (o : down_message)
          | Error e -> fail "unknown error")))
        
let client_onload div_id =
  let dbg = debug_service () in
  let caml = submission_caml_service () in
  {{
    let open Html5 in
    try begin
      let debug_local msg =
        Js.Opt.iter
          (Dom_html.document##getElementById(Js.string "client_debug_messages"))
          (fun d -> Dom.appendChild d
            (Eliom_client.Html5.of_div (div [msg]))) in
      debug_local (pcdataf "bouh"); 
      
      let call_caml msg =
        Eliom_client.call_caml_service ~service: %caml msg () in
      
      debugf %dbg "BEGIN onload for %S " %div_id;
      let initial_div =
        Js.coerce_opt 
          (Dom_html.document##getElementById (Js.string %div_id))
          Dom_html.CoerceTo.div (fun _ ->
            debugf %dbg "failed coercion for %S " %div_id;
            assert false) in
      let test_div =
        div ~a:[a_id "project_input"]
          [ pcdata "Please choose an existing project or create a new one: ";
            span ~a:[ a_id "select1"; (* a_style "min-width: 5em"; *)
                      a_class ["goog-menu"]] [];
            pcdata " choice: ";
            span ~a:[ a_id "value1" ] [];
          ]
          |! unique (* TODO check if really needed or not *)
      in
      let b = Eliom_client.Html5.of_div test_div in
      Dom.appendChild initial_div b;
      
      let () =
        let open Goog in
        let dom = jsnew Gdom.domHelper() in
        let get_el s =
          Js.Opt.get (Dom_html.document##getElementById (Js.string s))
            (fun _ ->
              debugf %dbg "Getting %S -> ERROR " s;
              assert false) in
        let add_item (select : Ui.select Js.t) name = 
          let item =
            jsnew Ui.menuItem(Ui.ControlContent.string (Js.string name),
                              Js.null,Js.null) in
          select##addItem_(Tools.Union.i1 item)
        in
        let select1 = jsnew Ui.select(
          Js.some (Ui.ControlContent.string (Js.string "Click to select project")),
          Js.null,Js.null,Js.null) in
        Lwt.bind (call_caml Request_project_list) (function
        | Project_list l ->
          List.iter (fun project_name -> add_item select1 project_name;) l;
          Lwt.return ()
        ) |! ignore;
        (* add_item select1 "Godfather Part II"; *)
        (* add_item select1 "Citizen Kane"; *)
        (* select1##setSelectedIndex(0); *)
        select1##render(Js.some (get_el "select1"));

        let listener select id sentence = 
          Events.listen
            (Tools.Union.i1 select)
            Ui.Component.EventType._ACTION
            (Js.wrap_callback (fun _ -> 
              let value =
                Js.string (sentence
                           ^ (Js.to_string (Js.Optdef.get select##getValue()
                                              (fun _ -> assert false))) ^ "!") in
	      dom##setTextContent(get_el id,value)  
             ))
            Js.null 
        in
        ignore(listener select1 "value1" "Yeah ");

      in


      
      (* 
         let menu_id = unique_id "menu" in
         let menu =
         div ~a:[ a_id menu_id; a_class ["goog-menu"]; a_tabindex 0] [
         div ~a:[ a_class ["goog-menuitem"] ] [pcdata "one"];
         div ~a:[ a_class ["goog-menuitem"] ] [pcdata "two"];
         div ~a:[ a_class ["goog-menuitem"] ] [pcdata "three"];
         hr ();
         div ~a:[ a_class ["goog-menuitem"] ] [pcdata "Create …"];
         ];
         |! unique
         in
         let select =
        (* http://ocsigen.org/oclosure/dev/api/Goog.Ui.ControlContent *)
         jsnew Goog.Ui.menu(
               (* Js.some (Goog.Ui.ControlContent.string (Js.string "choose project")), *)
               (* Js.null, *)
               (* Js.null, *)
         Js.null,
         Js.null)
         in
      (* select##addItem(jsnew Goog.Ui.menuSeparator(Js.null)); *)
      (* select##addItem(jsnew Goog.Ui.menuItem("New Folder...")); *)

         Dom.appendChild initial_div (Eliom_client.Html5.of_div menu);
         Js.Opt.iter (Dom_html.document##getElementById (Js.string menu_id))
         (fun menu_div ->
         debugf %dbg "decorate %S " menu_id;
         select##decorate(menu_div)
         ); 
     (* Dom.appendChild initial_div menu##getContentElement(); *)
      *)
      let test_div =
        div ~a:[a_id "some div id"]
          [ pcdata "some more clickable text";
            br ();
            pcdata "more other text"; ]
      in
      let b = Eliom_client.Html5.of_div test_div in
      b##onclick <- (Dom_html.handler (fun ev ->
        debugf %dbg "handler for %S.button " %div_id;
        Js._true));
      Dom.appendChild initial_div b;
      
      debugf %dbg "END of onload for %S " %div_id;
    end
    with e -> 
      debugf %dbg "Exception in onload for %S: %s" %div_id (Printexc.to_string e);
      ()
  }}

let start ~configuration ~main_title =
  let open Html5 in
  let id = "pool_submission_form" in
  Eliom_services.onload (client_onload id);
  return
    [ h1 [pcdata "Pool Submission Form" ];
      div ~a:[ a_id id] [] ]
  (* (error (`not_implemented "pools-submission")) *)
  
let make ~configuration =
  (fun () () ->
    let main_title = "Submit Pools" in
    Template.default ~title:main_title
      (Authentication.authorizes (`perform `pools_submission)
       >>= function
       | true -> start ~configuration ~main_title
       | false ->
         Template.make_authentication_error ~configuration ~main_title
           (return [Html5.pcdataf "You may not submit new libraries."])))
