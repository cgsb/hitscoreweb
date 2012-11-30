
{shared{
open Hitscoreweb_std
open Printf

type kind_key = string deriving (Json)

type phrase = string deriving (Json)
  
type ('a, 'b) item_value = V_some of 'a | V_none | V_wrong of 'b
deriving (Json)

type ('a, 'b) form_item = {
  question: phrase option;
  value: ('a, 'b) item_value;
}
deriving (Json)

module Range = struct
  type boundary = Inclusive of float | Exclusive of float | Infinity
    deriving (Json)

  let inclusive f = Inclusive f
  let exclusive f = Exclusive f
  let infinity = Infinity

  type t = { min: boundary; max: boundary }
    deriving (Json)

  let make min max = {min; max}
  let to_string r =
    sprintf "%s, %s"
      (match r.min with
      | Inclusive f -> sprintf "[ %g" f
      | Exclusive f -> sprintf "] %g" f
      | Infinity -> sprintf "] -∞")
      (match r.max with
      | Inclusive f -> sprintf "%g ]" f
      | Exclusive f -> sprintf "%g [" f
      | Infinity -> sprintf "∞ [")
  let check_float r f =
    (match r.min with
    | Inclusive m -> f >= m
    | Exclusive m -> f > m
    | Infinity -> true)
    && (match r.max with
    | Inclusive m -> f <= m
    | Exclusive m -> f < m
    | Infinity -> true)
end

    

type meta_enumeration = {
  overall_question: phrase option;
  default_cases: (string * form_content) list;
  creation_case: (string * form_content) option;
  choice: string option;
}
and extensible_list = {
  el_question: phrase;
  el_model: form_content;
  el_list: form_content list;
}
and form_content =
| String of (string, string) form_item
| Integer of (int, string) form_item
| Float of (float, string) form_item
| String_regexp of string * string * (string, string) form_item
| Meta_enumeration of meta_enumeration
| Extensible_list of extensible_list
| List of form_content list
| Section of string * form_content
| Empty
deriving (Json)

type form = {
  form_content: form_content;
  form_button: string;
}
deriving (Json)


module String_type = struct
  let to_string s = s
  let of_string s = `ok s
end
module String_regexp_type = struct
  let regexp_matches rex s =
    let t = Re_posix.re (sprintf "^%s$" rex) in
    let re = Re.compile t in
    Re.execp re s
    
  include String_type
  let of_string rex msg s =
    if regexp_matches rex s
    then `ok s
    else `error (sprintf "wrong format, expecting: %s" msg)
end
module Integer_type = struct
  let to_string i = sprintf "%d" i
  let of_string s =
    try `ok (int_of_string s)
    with _ -> `error "This is not an integer"
end
module Float_type = struct
  let to_string = string_of_float
  let of_string s =
    try `ok (float_of_string s)
    with _ -> `error "This is not a floating point number"
end
  
module Form = struct
  (* let item ?value question kind = Item {question; kind; value} *)
  let make_item ~f ?question ?value () =
    match value with
    | Some s -> f {question; value = V_some s}
    | None ->  f {question; value = V_none}
  let integer = make_item ~f:(fun x -> Integer x)
  let string ?regexp =
    match regexp with
    | Some (m, r) -> 
      make_item ~f:(fun x -> String_regexp (r, m, x))
    | None -> 
      make_item ~f:(fun x -> String x)

  let float = make_item ~f:(fun x -> Float x)
  let list l = List l
  let section s = function
    | [one] -> Section (s, one)
    | more_or_less -> Section (s, List more_or_less)
  let meta_enumeration ?overall_question ?creation_case ?choice default_cases =
    Meta_enumeration {
      overall_question;
      default_cases;
      creation_case;
      choice;
    }
  let string_enumeration ?question ?value l =
    meta_enumeration ?overall_question:question ?choice:value
      (List.map l ~f:(fun s -> (s, string ~value:s ())))

  let open_string_enumeration ?question ?value ?(other="New") l =
    meta_enumeration ?overall_question:question ?choice:value
      ~creation_case:(other, string ())
      (List.map l ~f:(fun s -> (s, string ~value:s ())))

  let extensible_list ~question ~model l =
    Extensible_list {el_question = question;
                     el_model = model;
                     el_list = l}

  let empty = Empty

  let make ?(save="[[Save]]") form_content =
    { form_content; form_button = save }

end
    
type up_message =
| Form_changed of form
| Ready
deriving (Json)
type down_message =
| Make_form of form
| Form_saved
| Server_error of string
deriving (Json)

type chunk =
  { index: int; total_number: int; content: string; }
deriving (Json)
    
type message = Up of chunk | Down of string
deriving (Json)
  

module Style = struct

  open Hitscoreweb_template
  open Html5

  let _my_style = Local_style.create ()
    
  let make_class name style =
    a_class [ Local_style.add_class _my_style ("meta_form" ^ name) style ]

  let section_block =
    make_class "section_block" [
      "border: #f00 solid 2px";
      "padding: 1em";
    ]
  let submit_button =
    make_class "subbutton" [
      "border: #00d solid 2px";
      "padding: 3px";
      "background-color: #ddd";
    ]
  let extensible_list_button =
    make_class "extensible_list_button" [
      "color: #050;";
      "background-color: #ecc";
    ]
      
  let () = Local_style.use _my_style

end

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

let start_server ~state ~form_content =
  let bus =
    Eliom_bus.create ~scope:Eliom_common.site Json.t<message> in
  let stream = Eliom_bus.stream bus in
  let current_packet = ref None in
  let check_up up =
    if up.index < 0 then error (`wrong_message up)
    else if up.index >= up.total_number
    then error (`wrong_message up) 
    else
      (match !current_packet with
      | None -> return ()
      | Some a ->
        if up.total_number <> Array.length a then error (`wrong_message up)
        else return ()) in
  Lwt.ignore_result begin
    let rec loop () =
      wrap_io Lwt_stream.get stream
      >>= begin function
      | Some (Up up) ->
        dbg "up: %d %d %d" up.index up.total_number (String.length up.content);
        check_up up >>= fun () ->
        begin match !current_packet with
        | None ->
          let a = Array.create up.total_number None in 
          current_packet := Some a;
          return a
        | Some a -> return a
        end
        >>= fun packet ->
        packet.(up.index) <- Some up.content;
        (if Array.for_all packet ((<>) None)
         then
            (let up_msg =
               String.concat_array ~sep:""
               (Array.map packet ~f:(Option.value_exn ?here:None ?error:None ?message:None))
                 in
             wrap ~on_exn:(fun e -> `json_parsing e)
               ~f:Deriving_Json.(from_string Json.t<up_message>) up_msg
             >>= fun msg ->
             (reply ~state form_content msg)
             >>= fun answer ->
             current_packet := None;
             let msg = Deriving_Json.(to_string Json.t<down_message> answer) in
             Eliom_bus.write bus (Down msg);
             loop ())
         else loop ())
      | Some (Down d) -> loop ()
      | None -> return ()
      end
    in
    Lwt.bind (loop ()) (function
    | Ok o ->
      logf "Meta_form: Server-side end of loop" |! Lwt.ignore_result;
      dbg "Meta_form: Server-side end of loop";
      Lwt.return ()
    | Error e ->
      let s =
          match e with
          | `json_parsing e -> sprintf "json_parsing: %s" (Exn.to_string e)
          | `wrong_message e ->
            sprintf "wrong_message: %s"
              Deriving_Json.(to_string Json.t<chunk> e)
          | other -> "OTHER"
      in
      logf "Meta_form: Server-side dies with error: %s" s |! Lwt.ignore_result;
      dbg "Meta_form: Server-side dies with error: %s" s;
      Lwt.return ())
  end;
  bus


{client{

open Html5
open Lwt 

let form_item (of_string, to_string) it =
  dbg "form_item";
  let value =
    match it.value with
    | V_some s -> to_string s
    | V_wrong s -> s
    | V_none -> "" in
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
          current_value := { !current_value with value = V_some v }
        | `error s ->
          msg_box##innerHTML <- ksprintf Js.string "KO : %s" s;
          current_value := { !current_value with value = V_wrong s }
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

let rec make_meta_enumeration me =
  let current_value = ref me in
  begin match me.creation_case with
  | Some (label, form) ->
    make_form form
    >>= fun (d, f) ->
    return (d, fun () -> Some (label, f ()))
  | None -> return (div [], fun () -> None)
  end
  >>= fun (creation_case_div, creation_case_fun) ->
  let set_creation_visibility b =
    let elt = Html5_to_dom.of_div creation_case_div in
    if b then elt##style##display <- Js.string "block"
    else elt##style##display <- Js.string "none"
  in
  let selection_handler the_chosen_one =
    current_value := {!current_value with choice = Some the_chosen_one };
    begin match me.creation_case with
    | Some (label, _) ->
      set_creation_visibility (the_chosen_one = label)
    | None -> ()
    end;
    ()
  in
  let the_hook_div = div [] in
  let hook_elt = Html5_to_dom.of_div the_hook_div in
  let select = Dom_html.createSelect Dom_html.document in
  let make_option ~is_selected choice =
    let label = Dom_html.createOption Dom_html.document in
    label##value <- Js.string choice;
    label##label <- Js.string choice;
    label##disabled <- Js._false;
    label##innerHTML <- Js.string choice;
    if is_selected then (
      label##defaultSelected <- Js._true;
    ) else (
      label##defaultSelected <- Js._false;
    );
    select##add(label, Js.null);
  in
  List.iter me.default_cases ~f:(fun (choice, _) ->
    let is_selected = Some choice = me.choice in
    make_option ~is_selected choice
  );
  begin match me.creation_case with
  | Some (choice, _) ->
    let is_selected = Some choice = me.choice in
    make_option ~is_selected choice;
    set_creation_visibility is_selected;
  | None -> ()
  end;
  select##onchange <- Dom_html.handler (fun ev ->
    dbg "select changes: %S" (Js.to_string select##value);
    selection_handler (Js.to_string select##value);
    Js._true
  );
  Dom.appendChild hook_elt select;
  let question_h5 =
    match me.overall_question with
    | Some q -> [div [ pcdata q; pcdata "  :  "; ]]
    | None -> [] in
  return (div (question_h5 @ [the_hook_div; creation_case_div]),
          fun () -> { !current_value with creation_case = creation_case_fun () })

and make_extensible_list el =
  Lwt_list.map_s make_form el.el_list
  >>= fun div_funs ->
  let make_new_button =
    div ~a:[ Style.extensible_list_button ] [pcdata el.el_question] in
  let make_new_button_elt = Html5_to_dom.of_div make_new_button in
  let additional_elements_div = div [] in
  let additional_elements_funs = ref [] in
  let additional_elements_elt = Html5_to_dom.of_div additional_elements_div in
  make_new_button_elt##onclick <- Dom_html.handler (fun ev ->
    dbg "make_new_button_elt clicked";
    Lwt.ignore_result begin
      make_form el.el_model
      >>= fun (new_div, new_fun) ->
      Dom.appendChild additional_elements_elt (Html5_to_dom.of_div new_div);
      (* for i = 0 to 40 do *)
      additional_elements_funs := new_fun :: !additional_elements_funs;
      (* done; *)
      return ()
    end;
    Js._true);
  let the_div =
    div (List.map div_funs ~f:fst
         @ [additional_elements_div; make_new_button]) in
  let the_fun () =
    let tmp_array =
      Array.create
        (List.length div_funs + List.length !additional_elements_funs)
        el.el_model in
    let idx = ref 0 in
    let f = (fun g -> tmp_array.(!idx) <- g (); incr idx) in
    List.iter div_funs ~f:(fun (_, g) -> f g);
    List.iter !additional_elements_funs ~f;
    { el with el_list = Array.to_list tmp_array } in
  return (the_div, the_fun)

and make_form f =
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
  | String_regexp (rex, msg, s) ->
    form_item String_regexp_type.(of_string rex msg, to_string) s
    >>= fun (the_div, the_fun) ->
    return (the_div, fun () -> String_regexp (rex, msg, the_fun ()))
  | Integer it ->
    form_item Integer_type.(of_string, to_string) it
    >>= fun (the_div, the_fun) ->
    return (the_div, fun () -> Integer (the_fun ()))
  | Float it ->
    form_item Float_type.(of_string, to_string) it
    >>= fun (the_div, the_fun) ->
    return (the_div, fun () -> Float (the_fun ()))
  | Meta_enumeration me ->
    make_meta_enumeration me
    >>= fun (the_div, the_fun) ->
    return (the_div, fun () -> Meta_enumeration (the_fun ()))
  | Section (title, content) ->
    make_form content
    >>= fun (the_div, the_fun) ->
    let d =
      div ~a:[ Style.section_block ]
        [ div [pcdataf "Section %S" title]; the_div] in
    return (d, fun () -> Section (title, the_fun ()))
  | Extensible_list el ->
    make_extensible_list el
    >>= fun (the_div, the_fun) ->
    return (the_div, fun () -> Extensible_list (the_fun ()))
  | Empty ->
    return (div [], fun () -> Empty)
  end



}}

let create ~state  form_content =
  let hook_id = unique_id "meta_form_hook" in
  let the_link_like =
    let open Html5 in
    span ~a:[a_id hook_id; a_class ["like_link"]]
      [pcdata "Meta-form Hook … Loading …"] in
  let bus = start_server ~state ~form_content  in
  Eliom_service.onload {{
    let open Html5 in
    let open Lwt in
    try
      begin

        (* dbg "Meta_form: onload (websockets: %b)" (WebSockets.is_supported ()); *)

        
        Lwt.async_exception_hook := (fun e ->
          dbg "Async-exn: %s" (Printexc.to_string e);
          Firebug.console##log(e));

        let server_stream = Eliom_bus.stream %bus in
        (* Eliom_bus.set_time_before_flush %bus 0.5; *)
        let bus = %bus in
        
        let call_server up =
          (* Eliom_client.call_caml_service ~service: %caml msg () in *)
          dbg "call_server: writing";
          let msg = Deriving_Json.(to_string Json.t<up_message> up) in
          dbg "up msg: (%d) %s" (String.length msg) msg;
          (* (if String.length msg > 5000 then *)
          (* Eliom_bus.write %bus (Up "") *)
          (* else *)
          let msg_length = String.length msg in
          begin if msg_length < 2000 then (
            Eliom_bus.write bus (Up {index = 0; total_number = 1; content = msg})
            >>= fun () ->
            return ()
          ) else (
            let total_number =
              msg_length / 1000 + (if msg_length / 1000 = 0 then 0 else 1) in
            dbg "lgth: %d, total_number: %d" msg_length total_number;
            Eliom_bus.set_queue_size bus (total_number + 1);
            let rec loopi index =
              if index = total_number - 1
              then (
                let ofset = index * 1000 in
                let size = msg_length - ofset in
                let content = String.sub msg ofset size in
                Eliom_bus.write bus (Up {index; total_number; content})
                >>= fun () ->
                return ()
              ) else (
                let ofset = index * 1000 in
                let size = 1000 in
                let content = String.sub msg ofset size in
                Eliom_bus.write bus (Up {index; total_number; content})
                >>= fun () ->
                (loopi (index + 1) : unit Lwt.t)
              ) in
            (loopi 0 : unit Lwt.t)
          )
          end
          >>= fun () ->
          dbg "call_server: wrote";
          let rec next_down () =
            Lwt_stream.get server_stream
            >>= begin function
            | Some (Down msg) ->
              dbg "call_server: down-msg";
              begin try
                      return Deriving_Json.(from_string Json.t<down_message> msg)
                with
                  e -> dbg "Exn: %s" Printexc.(to_string e);
                    return (Server_error "boooooojjjjhhhhjjj")
              end
            | Some (Up up) ->
              dbg "call_server: up-msg";
              next_down ()
            | None ->
              fail (Failure "server_stream ended")
            end in
          if (String.length msg) > 5000 then
            return (Make_form { form_content = Empty; form_button = "DUMMMY"})
          else
            next_down ()
        in



        let rec make_with_save_button send_to_server =
          let hook = get_element_exn %hook_id in
          hook##innerHTML <- Js.string "Contacting the server … ";
          (* List.iter (fun x -> dbg "class: %s" x.name) Kind.list; *)
          Lwt.ignore_result begin
            call_server send_to_server
            >>= begin function
            | Make_form f ->
              dbg "Make form?!";
              make_form f.form_content
              >>= fun (the_div, whole_function) ->
              let whole_form =
                let bdiv =
                  div ~a:[ Style.submit_button ] [ pcdata f.form_button ] in
                let belt = Html5_to_dom.of_div bdiv in
                belt##onclick <- Dom_html.handler(fun _ ->
                  let new_form = { f with form_content = whole_function () } in
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
        make_with_save_button Ready;
      end
    with e -> 
      dbg "Exception in onload for %S: %s" %hook_id (Printexc.to_string e);
      ()
  }};
  the_link_like
