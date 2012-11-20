
{shared{
open Hitscoreweb_std
open Printf

type kind_key = string deriving (Json)

type phrase = string deriving (Json)
  
type 'a form_item = {
  question: phrase option;
  value: 'a option;
}
deriving (Json)

type form =
| String of string form_item
| Integer of int form_item
| List of form list
| Section of string * form
deriving (Json)

module String_type = struct
  let to_string s = s
  let of_string s = `ok s
end
module Integer_type = struct
  let to_string i = sprintf "%d" i
  let of_string s =
    try `ok (int_of_string s)
    with _ -> `error "This is not an integer"
end

module Form = struct
  (* let item ?value question kind = Item {question; kind; value} *)
  let item ?question ?value () = {question; value}
  let integer ?question ?value () =
    Integer (item ?question ?value ())
  let string ?question ?value () =
    String (item ?question ?value ())
  let list l = List l
  let section s = function
    | [one] -> Section (s, one)
    | more_or_less -> Section (s, List more_or_less)
end
    
type up_message =
| Form_changed of form
| Ready
deriving (Json)
type down_message =
| Make_form of form
| Form_saved
| Server_error of string

}}


let reply ~state form_content param =
  match param with
  | Ready ->
    form_content None
    >>= fun f ->
    return (Make_form f)
  | Form_changed form ->
    form_content (Some form)
    >>= fun f ->
    return (Make_form f)

let caml_service ~state ~path ~form_content =
  let caml_service =
    make_delayed
      (Eliom_service.service ~path
         ~get_params:Eliom_parameter.(caml "param" Json.t<up_message>)) in
  let already = ref false in
  fun () ->
    if !already then caml_service () else (
      already := true;
      let fail fmt =
        ksprintf
          (fun s -> Lwt.fail (Failure ("wrong reply from server: " ^ s)))
          fmt
      in
      Eliom_registration.Ocaml.register ~service:(caml_service ())
        (fun param () ->
          Lwt.bind (reply ~state form_content param) (function
          | Ok o -> Lwt.return (o : down_message)
          | Error e -> fail "unknown error"));
      caml_service ()
    )

let create ~state ~path  form_content =
  let hook_id = unique_id "meta_form_hook" in
  let the_link_like =
    let open Html5 in
    span ~a:[a_id hook_id; a_class ["like_link"]]
      [pcdata "Meta-form Hook … Loading …"] in
  let caml = caml_service ~state ~path ~form_content () in
  Eliom_service.onload {{
    let open Html5 in
    let open Lwt in
    try
      begin
        
        let call_caml msg =
          Eliom_client.call_caml_service ~service: %caml msg () in

        let rec make_form f =
          let form_item (of_string, to_string) it =
            dbg "form_item";
            let value = match it.value with Some s -> to_string s | None -> "" in
            let potential_msg = span [] in
            let current_value = ref it in
            let update ev =
              let msg_box = Html5_to_dom.of_span potential_msg in
              Js.Optdef.iter (ev##target) (fun src_elt ->
                match Dom_html.tagged src_elt with
                | Dom_html.Input ielt ->
                  dbg "Input ELT %s" (Js.to_string ielt##value);
                  begin match of_string (Js.to_string ielt##value) with
                  | `ok v -> 
                    msg_box##innerHTML <-
                      ksprintf Js.string "OK: %s" (Js.to_string ielt##value);
                    current_value := { !current_value with value = Some v }
                  | `error s ->
                    msg_box##innerHTML <- ksprintf Js.string "KO : %s" s
                  end
                | _ ->
                  dbg "WRONG ELEMENT"
              );
              dbg "verification"
            in
            let question_h5 =
              match it.question with
              | Some q -> [div [ pcdata q; pcdata "  :  "; ]]
              | None -> [] in
            return (div (question_h5 @ [
              string_input ~a:[
                a_onchange update;
                a_onmouseup update;
                a_onkeyup update;
              ] ~input_type:`Text ~value ();
              potential_msg;
            ]), fun () -> !current_value)
          in
          begin match f with
          | List l ->
            let get_value_funs = ref [] in
            Lwt_list.map_s (fun f ->
              make_form f
              >>= fun (the_div, the_fun) ->
              get_value_funs := the_fun :: !get_value_funs;
              return the_div) l
            >>= fun ldivs ->
            return (div ldivs, fun () -> List (List.rev_map (fun f -> f ()) !get_value_funs))
          | String it ->
            form_item String_type.(of_string, to_string) it
            >>= fun (the_div, the_fun) ->
            return (the_div, fun () -> String (the_fun ()))
          | Integer it ->
            form_item Integer_type.(of_string, to_string) it
            >>= fun (the_div, the_fun) ->
            return (the_div, fun () -> Integer (the_fun ()))
          | Section (title, content) ->
            make_form content
            >>= fun (the_div, the_fun) ->
            let d =
              div [ div [pcdataf "Section %S" title]; the_div] in
            return (d, fun () -> Section (title, the_fun ()))
          end
        in

        let rec make_with_save_button send_to_server =
          let hook = get_element_exn %hook_id in
          hook##innerHTML <- Js.string "Contacting the server … ";
          (* List.iter (fun x -> dbg "class: %s" x.name) Kind.list; *)
          Lwt.ignore_result begin
            call_caml send_to_server
            >>= begin function
            | Make_form f ->
              dbg "Make form?!";
              make_form f
              >>= fun (the_div, whole_function) ->
              let whole_form =
                let bdiv = div [ pcdataf "[[SAVE]]" ] in
                let belt = Html5_to_dom.of_div bdiv in
                belt##onclick <- Dom_html.handler(fun _ ->
                  let new_form = whole_function () in
                  make_with_save_button (Form_changed new_form);
                  Js._true
                ); 
                div [the_div; bdiv]
              in
              let elt = Html5_to_dom.of_div whole_form in
              hook##innerHTML <- Js.string "Please fill the form: ";
              Dom.appendChild hook elt;
              return ()
            | Server_error s ->
              hook##innerHTML <- ksprintf Js.string "Server Error: %s" s;
              return ()
            | Form_saved ->
              hook##innerHTML <- ksprintf Js.string "Form saved, thank you.";
              return ()
            end
          end
        in
        make_with_save_button Ready
      end
    with e -> 
      dbg "Exception in onload for %S: %s" %hook_id (Printexc.to_string e);
      ()
  }};
  the_link_like
