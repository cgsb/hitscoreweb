
{shared{
module LL = ListLabels
module Html5 = struct
  include Eliom_content.Html5.D
  open Printf 
  let pcdataf fmt = ksprintf pcdata fmt

  let codef fmt = ksprintf (fun s -> code [pcdata s]) fmt

  let strongf fmt = ksprintf (fun s -> strong [pcdata s]) fmt

  let a_hreff fmt = ksprintf (fun s -> a_href (Xml.uri_of_string s)) fmt

  let core_a = Eliom_content_core.Html5.D.a

end


module Upload_shared = struct
  type t = int deriving (Json)
  let path = ["meta_form_upload"]
end
 }}
{client{
module Upload = Upload_shared
}}

module Upload = struct
  include Upload_shared
  open Hitscoreweb_std

  type file = {
    id: int;
    original_name: string;
    path: string;
  }
  let _table : (int, file list) Hashtbl.t = Int.Table.create ()


  let upload_fallback =
    make_delayed
      (Eliom_service.service ~path ~get_params:Eliom_parameter.unit)

  let get_file id =
    Option.value_map ~default:[] (Hashtbl.find _table id)
      ~f:(List.map ~f:(fun s -> (s.path, s.original_name)))
      
  let init =
    let is_done = ref false in
    begin fun () ->
      if not !is_done then (
        dbg "registering meta_form_upload";
        let open Lwt in
        let () =
          Eliom_registration.Html5.register
            ~service:(upload_fallback ()) (fun () () ->
              dbg "fall back service";
              return Html5.(html
                              (head (title (pcdata "Upload Fallback")) [])
                              (body [h1 [pcdata "Upload Fallback"]]))) in

        let (_: _) = 
          Eliom_registration.Html5.register_post_service ~fallback:(upload_fallback ())
            ~post_params:Eliom_parameter.(int "id" ** file "file")
            (fun () (id, file) ->
              dbg "coservice !";
              let newname =
                Filename.temp_file ~in_dir:"/tmp" ~perm:0o600
                  (sprintf "hsw_mfupload_%d_" id) "" in
              (try Unix.unlink newname; with _ -> ());
              dbg "file %d tmp filename: %s, orig %s, new: %s" id
                (Eliom_request_info.get_tmp_filename file)
                (Eliom_request_info.get_original_filename file) newname;
              Lwt_unix.link (Eliom_request_info.get_tmp_filename file) newname
              >>= fun () ->
              let local_file =
                {id; path = newname;
                 original_name = Eliom_request_info.get_original_filename file}
              in
              Hashtbl.add_multi _table id local_file;
              return Html5.(html
                              (head (title (pcdata "Upload")) [])
                              (body [h1 [pcdata "Upload"]])))
        in
        is_done := true)
    end

end

{client{
  open Hitscoreweb_std
}}
{shared{
open Printf

type kind_key = string deriving (Json)

module Markup = struct
  type phrase =
  | Text of string
  | Italic of phrase
      deriving (Json)
  type structure =
  | Paragraph of phrase list
  | List of structure list
      deriving (Json)

  let text s = Text s
  let italic s = Italic (Text s)
  let par l = Paragraph l
  let list l = List l

  let to_html t =
    let open Eliom_content.Html5.D in
    let rec simple =
      function
      | Text s -> span [pcdata s]
      | Italic t -> span [i [simple t]]
    in
    let rec not_simple = function
      | Paragraph p -> div (LL.map simple p)
      | List tl -> div [ul (LL.map tl ~f:(fun t -> li [(not_simple t)]))]
    in
    not_simple t
      
end
type phrase = Markup.phrase list deriving (Json)
type content = Markup.structure deriving (Json)
  
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
| Integer_range of Range.t * (int, string) form_item
| Float of (float, string) form_item
| Float_range of Range.t * (float, string) form_item
| String_regexp of string * string * (string, string) form_item
| Upload of phrase * Upload.t * bool
| Meta_enumeration of meta_enumeration
| Extensible_list of extensible_list
| List of form_content list
| Section of phrase * form_content
| Help of content * form_content
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
module Float_range_type = struct
  let to_string = string_of_float
  let of_string r s =
    try
      let f = float_of_string s in
      if Range.check_float r f then `ok f
      else `error (sprintf "This is out of range: %s" (Range.to_string r))
    with _ -> `error "This is not a floating point number"
end
module Integer_range_type = struct
  let to_string = string_of_int
  let of_string r s =
    try
      let f = int_of_string s in
      if Range.check_float r (float f) then `ok f
      else `error (sprintf "This is out of range: %s" (Range.to_string r))
    with _ -> `error "This is not a integer"
end
}}
  
(* Form construction module, on server side. *)
module Form = struct
  (* let item ?value question kind = Item {question; kind; value} *)
  let with_help help content = Help (help,content)
  let optional_help ?help content =
    match help with None -> content | Some c -> Help (c, content)

  let uploads = ref 0
  let upload ?(multiple=true) q =
    incr uploads;
    Upload (q, !uploads, multiple)
    
  let make_item ~f ?help ?question ?text_question ?value () =
    let question =
      match question, text_question with
      | Some q, _ -> Some q
      | None, Some t -> Some Markup.([text t])
      | None, None -> None in
    let m i =
      match help with
      | Some s -> Help (s, f i)
      | None -> f i in
    match value with
    | Some s -> m {question; value = V_some s}
    | None   -> m {question; value = V_none}

  let string ?regexp =
    match regexp with
    | Some (m, r) -> 
      make_item ~f:(fun x -> String_regexp (r, m, x))
    | None -> 
      make_item ~f:(fun x -> String x)

  let integer ?range =
    match range with
    | None -> make_item ~f:(fun x -> Integer x)
    | Some r -> make_item ~f:(fun x -> Integer_range (r, x))
  let float ?range =
    match range with
    | None -> make_item ~f:(fun x -> Float x)
    | Some r -> make_item ~f:(fun x -> Float_range (r, x))
  let list l = List l
  let section s = function
    | [one] -> Section (s, one)
    | more_or_less -> Section (s, List more_or_less)

  let meta_enumeration
      ?help ?overall_question ?creation_case ?choice default_cases =
    let me = 
      Meta_enumeration {overall_question; default_cases; creation_case;
                        choice;} in
    optional_help ?help  me

  let string_enumeration ?question ?value l =
    meta_enumeration ?overall_question:question ?choice:value
      (LL.map l ~f:(fun s -> (s, string ~value:s ())))

  let open_string_enumeration ?question ?value ?(other="New") l =
    meta_enumeration ?overall_question:question ?choice:value
      ~creation_case:(other, string ())
      (LL.map l ~f:(fun s -> (s, string ~value:s ())))

  let extensible_list ~question ~model l =
    Extensible_list {el_question = question;
                     el_model = model;
                     el_list = l}

  let empty = Empty

  let make ?(save="[[Save]]") form_content =
    { form_content; form_button = save }

end
    
{shared{
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

let packetize msg ~packet_size = 
  let msg_length = String.length msg in
  begin if msg_length < packet_size
    then ([| {index = 0; total_number = 1; content = msg} |])
    else begin
      let total_number =
        msg_length / packet_size + (if msg_length / packet_size = 0 then 0 else 1) in
      let result =
        let fake_one = {index = -1; total_number; content = ""} in
        Array.create total_number fake_one in
      (* dbg "lgth: %d, total_number: %d" msg_length total_number; *)
      for index = 0 to total_number - 2 do
        (* dbg "index: %d" index; *)
        let ofset = index * packet_size in
        let size = packet_size in
        let content = String.sub msg ofset size in
        result.(index) <- {index; total_number; content};
      done;
      let index = total_number - 1 in
      let ofset = index * packet_size in
      let size = msg_length - ofset in
      let content = String.sub msg ofset size in
      result.(index) <- {index; total_number; content};
      result
    end
  end

let unpacketizer () = ref None
     
let digest_chunk unpacketizer chunk =
  let check_chunk chunk =
    if chunk.index < 0 then (`wrong_chunk chunk)
    else if chunk.index >= chunk.total_number
    then (`wrong_chunk chunk) 
    else
      (match !unpacketizer with
      | None -> `ok
      | Some a ->
        if chunk.total_number <> Array.length a then (`wrong_chunk chunk)
        else `ok) in
  match check_chunk chunk with
  | `ok ->
    let unpack_array =
      match !unpacketizer with
      | None ->
        let a = Array.create chunk.total_number None in 
        unpacketizer := Some a;
        a
      | Some a -> a in
    unpack_array.(chunk.index) <- Some chunk.content;
    if Array.fold_left (fun b a -> b && (a <> None)) true unpack_array
    then begin
      let total_size =
        Array.fold_left (fun b -> function
        | Some a -> b + String.length a
        | None -> failwith  "Digest_chunk: should not be here") 0 unpack_array in 
      let packet = String.make total_size 'B' in
      let ofset = ref 0 in
      Array.iter (function
      | Some chunk_content ->
        String.blit chunk_content 0 packet !ofset (String.length chunk_content);
        ofset := !ofset + String.length chunk_content;
      | None -> failwith "Digest_chunk: should not be here") unpack_array;
      unpacketizer := None;
      `ready packet
    end else begin
      `not_ready
    end
  | `wrong_chunk _ as any -> any

type message = Up of chunk | Down of chunk
deriving (Json)
  
open Hitscoreweb_std

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
  let help_block =
    make_class "help_block" [
      "border: #000 solid 1px";
      "padding: 1em";
      "float: right;";
      "background-color: #ddd";
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
    (* Eliom_bus.create ~scope:Eliom_common.site Json.t<message> in *)
    Eliom_bus.create ~scope:Eliom_common.client_process Json.t<message> in
  let stream = Eliom_bus.stream bus in
  let unpacketizer = unpacketizer () in
  Lwt.ignore_result begin
    let rec loop () =
      wrap_io Lwt_stream.get stream
      >>= begin function
      | Some (Up up) ->
        dbg "up: %d %d %d" up.index up.total_number (String.length up.content);
        begin match digest_chunk unpacketizer up with
        | `not_ready -> loop ()
        | `ready up_msg ->
          wrap ~on_exn:(fun e -> `json_parsing e)
            ~f:Deriving_Json.(from_string Json.t<up_message>) up_msg
          >>= fun msg ->
          (reply ~state form_content msg)
          >>= fun answer ->
          let msg = Deriving_Json.(to_string Json.t<down_message> answer) in
          let chunks = packetize msg ~packet_size:500 in
          Array.iter chunks ~f:(fun chunk ->
            Eliom_bus.write bus (Down chunk);
          );
          loop ()
        | `wrong_chunk c -> error (`wrong_chunk c)
        end
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
          | `wrong_chunk e ->
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
    | Some q -> [div [ Markup.(to_html (par q)) ]]
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
    | Some q -> [div [ Markup.(to_html (par q)) ]]
    | None -> [] in
  return (div (question_h5 @ [the_hook_div; creation_case_div]),
          fun () -> { !current_value with creation_case = creation_case_fun () })

and make_extensible_list el =
  Lwt_list.map_s make_form el.el_list
  >>= fun div_funs ->
  let make_new_button =
    div ~a:[ Style.extensible_list_button ]
      [Markup.(to_html (par el.el_question))] in
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
  | Float_range (r, it) ->
    form_item Float_range_type.(of_string r, to_string) it
    >>= fun (the_div, the_fun) ->
    return (the_div, fun () -> Float_range (r, the_fun ()))
  | Integer_range (r, it) ->
    form_item Integer_range_type.(of_string r, to_string) it
    >>= fun (the_div, the_fun) ->
    return (the_div, fun () -> Integer_range (r, the_fun ()))

  | Upload (question, id, multiple) ->
    let the_div = div [ Markup.(to_html (par question))] in
    let input =
      Dom_html.createInput ~_type:(Js.string "file") Dom_html.document in
    let the_elt = Html5_to_dom.of_div the_div in
    if multiple
    then input##setAttribute(Js.string "multiple", Js.string "multiple");
    input##onchange <- Dom_html.handler (fun ev ->
      begin match Js.Optdef.to_option input##files with
      | Some s ->
        for i = 0 to s##length - 1 do
          let file = Js.Opt.get s##item(i) (fun () -> failwith "aaaahh") in
          dbg "input-file onchange: %d files, %dth: %s, %d" s##length
            i (Js.to_string file##name) (file##size);
          let form_contents =
            (* http://ocsigen.org/js_of_ocaml/dev/api/Form *)
            let zero = Form.empty_form_contents () in
            Form.append zero ("id", `String (ksprintf Js.string "%d" id));
            Form.append zero ("file", `File file);
            zero
          in
          let open Url in
          begin match Current.get () with
          | Some (Https { hu_host; hu_port })
          | Some (Http { hu_host; hu_port }) ->
            let url =
              Https { hu_host; hu_port; hu_fragment = "";
                      hu_path = Upload.path;
                      hu_path_string = String.concat "/" Upload.path;
                      hu_arguments = []} in
            dbg "NEW URL: %s" (Url.string_of_url url);
            Lwt.ignore_result XmlHttpRequest.(
              (* http://ocsigen.org/js_of_ocaml/dev/api/XmlHttpRequest *)
              perform_raw_url ~form_arg:form_contents (Url.string_of_url url)
              >>= fun http_frame ->
              dbg "http_frame: %s %d\ncontent: %S" http_frame.url
                http_frame.code http_frame.content;
              return ())

          | Some u ->
            dbg "URL: %s" (Url.string_of_url u)
          | None -> dbg "NO URL"
          end;
        done
      | None ->
        dbg "input-file onchange: no files";
      end;
      Js._true);
    Dom.appendChild the_elt input;
    return (the_div, fun () -> Upload (question, id, multiple))

  | Meta_enumeration me ->
    make_meta_enumeration me
    >>= fun (the_div, the_fun) ->
    return (the_div, fun () -> Meta_enumeration (the_fun ()))
  | Section (title, content) ->
    make_form content
    >>= fun (the_div, the_fun) ->
    let d =
      div ~a:[ Style.section_block ]
        [ div [Markup.(to_html (par title))]; the_div] in
    return (d, fun () -> Section (title, the_fun ()))
  | Help (help, content) ->
    make_form content
    >>= fun (the_div, the_fun) ->
    let help_div = div ~a:[ Style.help_block ] [Markup.(to_html help)] in
    let d = div [ the_div; help_div ] in
    let elt = Html5_to_dom.of_div d in
    let helt = Html5_to_dom.of_div help_div in
    helt##style##display <- Js.string "none";
    elt##onmouseover <- Dom_html.handler (fun ev ->
      helt##style##display <- Js.string "block";
      Js._true);
    elt##onmouseout <- Dom_html.handler (fun ev ->
      helt##style##display <- Js.string "none";
      Js._true);
    return (d, fun () -> Help (help, the_fun ()))
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
  Upload.init ();
  let bus = start_server ~state ~form_content  in
  Eliom_service.onload {{
    let open Html5 in
    let open Lwt in
    try
      begin

        
        Lwt.async_exception_hook := (fun e ->
          dbg "Async-exn: %s" (Printexc.to_string e);
          Firebug.console##log(e));

        let server_stream = Eliom_bus.stream %bus in
        (* Eliom_bus.set_time_before_flush %bus 0.5; *)
        let bus = %bus in
        
        let unpacketizer = unpacketizer () in
        let call_server up =
          let msg = Deriving_Json.(to_string Json.t<up_message> up) in
          dbg "up msg: (%d) %s" (String.length msg) msg;
          let packets = packetize msg ~packet_size:500 in
          Eliom_bus.set_queue_size bus (Array.length packets + 1);
          Array.fold_left (fun m c -> m >>= fun () -> Eliom_bus.write bus (Up c))
            (return ()) packets
          >>= fun () ->
          let rec next_down () =
            Lwt_stream.get server_stream
            >>= begin function
            | Some (Down chunk) ->
              (* dbg "call_server: down-msg"; *)
              begin match digest_chunk unpacketizer chunk with
              | `ready msg ->
                begin
                  try
                    return Deriving_Json.(from_string Json.t<down_message> msg)
                  with
                    e ->
                      dbg "Exn: %s\n\n%s\n" Printexc.(to_string e) msg;
                      return (Server_error "WRONG MESSAGE FROM SERVER")
                end
              | `not_ready ->
                (* dbg "call_server: `not_ready"; *)
                next_down ()
              | `wrong_chunk c ->
                dbg "call_server: `wrong_chunk: %d %d %s"
                  c.index c.total_number c.content;
                return (Server_error "WRONG MESSAGE CHUNK FROM SERVER")
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
