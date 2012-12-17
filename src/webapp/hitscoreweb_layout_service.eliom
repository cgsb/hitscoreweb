{shared{
open Hitscoreweb_std
}}

module Queries = Hitscoreweb_queries

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

module LDSL = Hitscoregen_layout_dsl


{shared{
type up_message =
| Add_record of string * string
| Add_function of string * string 
| Add_volume of string * string
| Modify_value of int * string * string
| Modify_volume of int * string * string
deriving (Json)

type down_message =
| Success
| Error_string of string

}}


let caml_service =
  make_delayed (Eliom_service.service 
          ~path:["layout_caml_service"]
          ~get_params:Eliom_parameter.(caml "param" Json.t<up_message>))

let check_access_type_and_sexp t content =
  Authentication.restrict_access (`edit `layout) >>= fun () ->
  (try return (Sexp.of_string content) with e -> error (`sexp_syntax e))
  >>= fun sexp ->
  of_result (Verify_layout.check_sexp t sexp) >>= fun () ->
  return sexp

let execute_query_and_succeed ~configuration query =
  with_database ~configuration (Backend.query query)
  >>= fun _ ->
  return Success

let reply ~configuration =
  function
  | Add_record (t, content) ->
    check_access_type_and_sexp t content >>= fun sexp ->
    let query = Sql_query.add_value_sexp ~record_name:t sexp in
    execute_query_and_succeed ~configuration query
  | Add_function (t, content) ->
    check_access_type_and_sexp t content >>= fun sexp ->
    let query =
      Sql_query.add_evaluation_sexp ~status:"Inserted" ~function_name:t sexp in
    execute_query_and_succeed ~configuration query
  | Add_volume (kind, content) ->
    check_access_type_and_sexp kind content >>= fun sexp ->
    let query =
      Sql_query.add_volume_sexp ~kind sexp in
    execute_query_and_succeed ~configuration query
  | Modify_value (id, t, content) ->
    check_access_type_and_sexp t content >>= fun sexp ->
    let query = Sql_query.update_value_sexp ~record_name:t id sexp in
    execute_query_and_succeed ~configuration query
  | Modify_volume (id, t, content) ->
    check_access_type_and_sexp t content >>= fun sexp ->
    let query = Sql_query.update_volume_sexp ~kind:t id sexp in
    execute_query_and_succeed ~configuration query

let init_caml_service ~configuration =
  let already = ref false in
  fun () ->
    if !already then () else (
      already := true;
      let fail fmt =
        ksprintf
          (fun s -> Lwt.return (Error_string ("Error: " ^ s)))
          fmt
      in
      Eliom_registration.Ocaml.register ~service:(caml_service ())
        (fun param () ->
          Lwt.bind (reply ~configuration param) (function
          | Ok o -> Lwt.return (o : down_message)
          | Error `not_implemented -> Lwt.return (Error_string "Not implemented")
          | Error (`sexp_syntax e) -> fail "S-Expr syntax: %s" (Exn.to_string e)
          | Error (`wrong_rights) ->
            fail "Wrong access rights! What the F#!$ are you trying to do?"
          | Error (`parse_sexp_error (_, e)) ->
            fail "S-Expr does not fit the type: %s" (Exn.to_string e)
          | Error e -> fail "unknown error")))


{client{

let sexp_area ?value () =
  Html5_to_dom.of_textarea
    Html5.(raw_textarea ~a:[ a_cols 50; a_rows 20] ?value ~name:"sexpinput" ()) 

let submit_button ?(visible=true) f =
  let open Html5 in
  let submit =
    Html5.button
      ~a:(if visible then [] else [a_style "visibility:hidden"])
      ~button_type:`Button [pcdata "submit"] in
  let btn_elt = Html5_to_dom.of_button submit in
  btn_elt##onclick <- Dom_html.handler (fun ev ->
    Lwt.ignore_result (f ev btn_elt); Js._true);
  btn_elt

}}

let add_or_modify_sexp_interface
    ?(modify: (int * string) option)
    (kind: [`Function| `Record| `Volume]) (type_name: string) =
  let (link_like_id: string) = unique_id "add_sexp" in
  let the_link_like =
    let open Html5 in
    [span ~a:[a_id link_like_id; a_class ["like_link"]]
        (if modify = None then
            [pcdataf "You may add a new %s" type_name]
         else 
            [pcdataf "You may modify this %s" type_name])] in
  let caml = caml_service () in
  let (type_name: string) = "" ^ type_name in
  (* the "" ^ _ is to please js_of_eliom's typing issues *)
  ignore {unit{
    let open Html5 in
    let open Lwt in
    let open Printf in
    try
      begin
        let call_caml msg =
          Eliom_client.call_caml_service ~service: %caml msg () in
        
        let the_span = get_element_exn %link_like_id in
        the_span##onclick <-
          Dom_html.(handler (fun ev ->
            the_span##onclick <- Dom_html.(handler (fun ev -> Js._true));
            the_span##innerHTML <-
              Js.string "Please, enter the S-Expression:<br/>";
            the_span##classList##remove(Js.string "like_link"); 

            let rec on_submit_sexp txt_elt ev btn_elt =
              the_span##innerHTML <- Js.string "<b>Processing …</b>";
              let sexp_str = Js.to_string txt_elt##value in
              begin match %kind , %modify with
              | `Record, None -> call_caml (Add_record ( %type_name, sexp_str))
              | `Function, None -> call_caml (Add_function ( %type_name, sexp_str)) 
              | `Volume, None -> call_caml (Add_volume ( %type_name, sexp_str))
              | `Record, Some (id, _) ->
                call_caml (Modify_value (id, %type_name, sexp_str)) 
              | `Volume, Some (id, _) ->
                call_caml (Modify_volume (id, %type_name, sexp_str)) 
              | _ -> fail (Failure "Not implemented …")
              end
              >>= fun msg ->
              begin match msg with
              | Success ->
                the_span##innerHTML <- Js.string "<b>Done.</b>";
                reload ()
              | Error_string s ->
                dbg "Got Error: %S" s;
                the_span##innerHTML <-
                  ksprintf Js.string "<b>Error: %s</b><br/>For %S<br/>" s sexp_str;
                make_interface ~value:sexp_str ();
                return ()
              end
            and make_interface ?value () =
              let txt_elt = sexp_area ?value () in
              let btn_elt = submit_button (on_submit_sexp txt_elt) in
              Dom.appendChild the_span txt_elt;
              Dom.appendChild the_span btn_elt;
            in
            begin match %modify with
            | None -> make_interface ();
            | Some (_, s) -> make_interface ~value:s ();
            end;
            Js._true
          ));
        
      end
    with e -> 
      dbg "Exception in onload for %S: %s" %link_like_id (Printexc.to_string e);
      ()
  }};
  the_link_like


  
let node_name = 
  let open LDSL in
  let open Html5 in
  let s color text =
    span ~a:[ ksprintf a_style "color: %s; font-weight: bold" color ]
      [pcdata text] in
  function
  | Enumeration (name, values) ->  (s "black" "Enumeration", name)
  | Record (name, typed_values) -> (s "red" "Record", name)
  | Function (name, args, res)  -> (s "blue" "Function", name)
  | Volume (name, toplevel) ->     (s "green" "Volume", name)

let self_link = function
  | `default ->
    fun html -> Template.a_link Services.layout [html]  ([], []) 
  | `view_one_type t ->
    fun html -> Template.a_link Services.layout [html]  ([t], []) 
  | `view_one_value (t, n) ->
    fun html -> Template.a_link Services.layout [html]  ([t], [n]) 

let node_link n = 
  let open Html5 in
  let styled_type, name = node_name n in
  span [pcdata "["; styled_type; pcdata " ";
        self_link (`view_one_type name) Html5.(codef "%s" name);
        pcdata "]"]

let find_node dsl nodename =
  let open LDSL in
  List.find dsl.nodes (function
  | Enumeration (name,_) -> name = nodename
  | Record (name, typed_values) -> nodename = name
  | Function (name, args, res)  -> nodename = name
  | Volume (name, toplevel) -> nodename = name
  )

let find_enumeration_values name =
  let open LDSL in
  List.find_map (Meta.layout ()).nodes 
    (function
    | Enumeration (enum, vals) when enum = name -> Some vals
    | _ -> None)


let typed_values_in_table l =
  let open Html5 in
  List.map l (fun (n, t) ->
    `head [
      b [codef "%s" n]; codef ": "; br ();
      i [codef "%s" (LDSL.string_of_dsl_type t)]]) 

let get_all_values ?(only=[])  dbh record_name =
  begin match only with
  | [] -> 
    let query = Sql_query.get_all_values_sexp ~record_name in
    Backend.query ~dbh query
    >>= fun results ->
    while_sequential results ~f:(fun row ->
      of_result (Sql_query.parse_value row))
  | a_bunch ->
    while_sequential a_bunch (fun id ->
      let query = Sql_query.get_value_sexp ~record_name id in
      Backend.query ~dbh query
      >>= fun r -> of_result (Sql_query.should_be_single r)
      >>= fun r -> of_result (Sql_query.parse_value r))
  end
let get_all_evaluations ?(only=[])  dbh function_name =
  begin match only with
  | [] -> 
    let query = Sql_query.get_all_evaluations_sexp ~function_name in
    Backend.query ~dbh query
    >>= fun results ->
    while_sequential results ~f:(fun row ->
      of_result (Sql_query.parse_evaluation row))
  | a_bunch ->
    while_sequential a_bunch (fun id ->
      let query = Sql_query.get_evaluation_sexp ~function_name id in
      Backend.query ~dbh query
      >>= fun r -> of_result (Sql_query.should_be_single r)
      >>= fun r -> of_result (Sql_query.parse_evaluation r))
  end
let get_all_volumes ~configuration ?(only=[]) dbh volume_kind =
  let enhance_volume v =
    let open Layout.File_system in
    Common.path_of_volume ~dbh ~configuration (unsafe_cast v.Sql_query.v_id)
    >>= fun path ->
    return (v, path)
  in
  begin match only with
  | [] -> 
    let query = Sql_query.get_all_volumes_sexp () in
    Backend.query ~dbh query
    >>= fun results ->
    while_sequential results ~f:(fun row ->
      of_result (Sql_query.parse_volume row)
      >>= fun v ->
      (if v.Sql_query.v_kind = volume_kind then return (Some v) else return None)
      >>= fun opt ->
      map_option opt enhance_volume)
    >>| List.filter_opt
  | a_bunch ->
    while_sequential a_bunch (fun id ->
      let query = Sql_query.get_volume_sexp id in
      Backend.query ~dbh query
      >>= fun r -> of_result (Sql_query.should_be_single r)
      >>= fun r -> of_result (Sql_query.parse_volume r)
      >>= enhance_volume)
  end
 

let generic_to_table type_info current_name r = []

let sortable_timestamp t = Template.cell_timestamp t
let sortable_text s =  Template.cell_text s
let sortable_link s t =
  let open Html5 in
  `sortable (s, [self_link (`view_one_value (t, Int.of_string s))
                    (pcdataf "%s" s)])

let find_in_sexp s field =
  let open Sexp in
  match s with
  | List l ->
    List.find_map l (function
    | List [Atom a; v] when a = field -> Some v
    | _ -> None)
  | _ -> None
    
let rec display_typed_value =
  let open Html5 in
  let open LDSL in
  let open Sexp in
  function
  | Bool, Atom s 
  | Int, Atom s
  | Real, Atom s
  | Enumeration_name _, Atom s
  | String, Atom s
    -> sortable_text s
  | Option t, List [] -> sortable_text ""
  | Option t, List [v] -> display_typed_value (t,v)
  | Array t, List l ->
    `sortable (l |! List.length |! Int.to_string,
               List.map l (fun v ->
                 match display_typed_value (t,v) with
                 | `sortable (_, p) -> p
                 | _ -> [])
               |! List.concat
               |! interleave_list ~sep:(pcdata ", "))

  | Record_name   name, List [List [Atom "id"; Atom s]]
  | Volume_name   name, List [List [Atom "id"; Atom s]]
  | Function_name name, List [List [Atom "id"; Atom s]]
    -> sortable_link s name
    
  | Timestamp,  l ->
    sortable_timestamp Timestamp.(t_of_sexp l)
  | _ -> sortable_text "«ERROR»"
  
    
let values_to_table type_info r =
  try
    List.map r ~f:(fun value ->
      let open Sql_query in
      (* eprintf "record %d\n%!" value.r_id; *)
      sortable_link (string_of_int value.r_id) value.r_type
      (* :: sortable_text value.r_type *)
      :: sortable_timestamp  value.r_created
      :: sortable_timestamp  value.r_last_modified
      :: sortable_text (Sexp.to_string_hum value.r_sexp)
      :: (List.map type_info (fun (field, field_type) ->
        Option.value_map
          ~default:(`sortable ("",[Html5.codef "—"]))
          ~f:(fun v -> display_typed_value (field_type, v))
          (find_in_sexp value.r_sexp field))))
  with e -> [[ `head [Html5.pcdataf "ERROR: %S" (Exn.to_string e)]]]
let evaluations_to_table type_info result_type r =
  try
    List.map r ~f:(fun eval ->
      let open Sql_query in
      let default = (`sortable ("",[Html5.codef "—"])) in
      sortable_link (string_of_int eval.f_id) eval.f_type
      :: Option.value_map eval.f_result ~default
        ~f:(fun i -> sortable_link (string_of_int i) result_type)
      :: sortable_timestamp eval.f_inserted  (*  Timestamp.t *)
      :: Option.value_map ~f:sortable_timestamp eval.f_started ~default
      :: Option.value_map ~f:sortable_timestamp eval.f_completed ~default
      :: sortable_text (status_to_string eval.f_status)  (*  status *)
      :: sortable_text (Sexp.to_string_hum eval.f_sexp)  (*  Sexp.t *)
      :: (List.map type_info (fun (field, field_type) ->
        Option.value_map
          ~default
          ~f:(fun v -> display_typed_value (field_type, v))
          (find_in_sexp eval.f_sexp field))))
  with e -> [[ `head [Html5.pcdataf "ERROR: %S" (Exn.to_string e)]]]
  
let volumes_to_table name toplevel r = 
  try
    List.map r ~f:(fun (vol, path) ->
      let open Sql_query in
      sortable_link (string_of_int vol.v_id) vol.v_kind
      :: sortable_text (Sexp.to_string_hum vol.v_sexp)
      :: sortable_text path
      :: [])
  with e -> [[ `head [Html5.pcdataf "ERROR: %S" (Exn.to_string e)]]]


let view_layout ~configuration ~main_title ~types ~values =
  let content =
    db_connect configuration >>= fun dbh ->

    let layout = Meta.layout () in
    let nodes =
      Html5.(div ~a:[ a_style "max-width: 55em" ]
               (List.map layout.LDSL.nodes node_link
                |! interleave_list ~sep:(pcdata ", "))) 
    in 
    let open Template in
    let open Html5 in
    let open LDSL in
    let table_section meta_values name table =
      let kind =
        match meta_values with
        | `Record _ -> `Record
        | `Function -> `Function
        | `Volume _ -> `Volume in
      Authentication.authorizes (`edit `layout)
      >>= fun can_edit ->
      let editors_paragraph =
        if can_edit then
          content_paragraph [
            pcdataf "Actions: ";
            ul (List.concat [
              add_or_modify_sexp_interface kind name;
              begin match meta_values with
              | `Record [one] ->
                let modify = Sql_query.(one.r_id, one.r_sexp |! Sexp.to_string) in
                add_or_modify_sexp_interface ~modify kind name
              | `Volume [one, _] ->
                let modify = Sql_query.(one.v_id, one.v_sexp |! Sexp.to_string) in
                add_or_modify_sexp_interface ~modify kind name
              | _ -> []
              end;
            ] |! List.map ~f:(fun l -> li [l]));
          ]
        else
          content_list []
      in
      let kindstr =
        match kind with
        | `Record -> "Record"
        | `Function -> "Function"
        | `Volume -> "Volume" in
      return (
        content_section (span [pcdata kindstr; codef " %s" name])
          (content_list [
            editors_paragraph;
            content_table ~transpose:(List.length values = 1) table;
          ]))
    in
    while_sequential types ~f:(fun elt ->
      match find_node layout elt with
      | Some s -> 
        begin match s with
        | Enumeration (name, values) ->
          content_section (span [pcdata "Enumeration "; codef "%s" name])
            (content_paragraph 
               (List.map values (codef "`%s") |! interleave_list ~sep:(br ())))
          |! return

        | Record (name, typed_values) ->
          get_all_values ~only:values dbh name >>= fun actual_values ->
          return (values_to_table typed_values actual_values)
          >>= fun table ->
          table_section (`Record actual_values) name
            (typed_values_in_table (record_standard_fields @ ["S-Exp", String]
                                    @ typed_values) :: table)

        | Function (name, args, res)  ->
          get_all_evaluations ~only:values dbh name
          >>| evaluations_to_table args res
          >>= fun table ->
          let all_typed =
            function_standard_fields res @ ["S-Exp", String] @ args in
          table_section `Function name (typed_values_in_table all_typed :: table)
        | Volume (name, toplevel) ->
          get_all_volumes ~configuration ~only:values dbh name
          >>= fun actual_volumes ->
          return (volumes_to_table name toplevel actual_volumes)
          >>= fun table ->
          let head s = List.map s (fun s -> `head [pcdataf "%s" s]) in
          table_section (`Volume actual_volumes) name
            (head ["Id"; "S-Exp"; "Path"] :: table)
        end
      | None -> 
        return (
          content_section (span [pcdata "Element "; codef "%s" elt])
            (content_paragraph
               [pcdataf "The element %S was not found" elt]))
    )
    >>= fun displayed_nodes ->
    db_disconnect configuration dbh >>= fun () -> 
    return (content_list (
      [content_section (pcdataf "The Layout") (content_paragraph [nodes]);] 
      @ displayed_nodes
    ))
  in
  Template.make_content ~configuration ~main_title content 

    
let make ~configuration =
  (fun (types, values) () ->
    let main_title = "The Layout Navigaditor" in
    let m =
      Authentication.authorizes (`view `layout)
      >>= function
      | true -> view_layout ~configuration ~main_title ~types ~values
      | false ->
        Template.make_authentication_error ~configuration ~main_title
          (return [Html5.pcdataf "You may not view the whole Layout."]) in
    Template.default ~title:main_title
      (bind_on_error m (fun e -> error (`Layout_service e))))
