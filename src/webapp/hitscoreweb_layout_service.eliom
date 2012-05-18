{shared{
open Hitscoreweb_std
}}

module Queries = Hitscoreweb_queries

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

module LDSL = Hitscoregen_layout_dsl

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
    fun html -> Template.a_link Services.layout [html] ("view", ([], [])) 
  | `view_one_type t ->
    fun html -> Template.a_link Services.layout [html] ("view", ([t], [])) 
  | `view_one_value (t, n) ->
    fun html -> Template.a_link Services.layout [html] ("view", ([t], [n])) 
  | `add_one_value t ->
    fun html -> Template.a_link Services.layout [html] ("edit", ([t], [])) 
  | `edit_one_value (t, n) ->
    fun html -> Template.a_link Services.layout [html] ("edit", ([t], [n])) 

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
    of_list_sequential results ~f:(fun row ->
      of_result (Sql_query.parse_value row))
  | a_bunch ->
    of_list_sequential a_bunch (fun id ->
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
    of_list_sequential results ~f:(fun row ->
      of_result (Sql_query.parse_evaluation row))
  | a_bunch ->
    of_list_sequential a_bunch (fun id ->
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
    of_list_sequential results ~f:(fun row ->
      of_result (Sql_query.parse_volume row)
      >>= fun v ->
      (if v.Sql_query.v_kind = volume_kind then return (Some v) else return None)
      >>= fun opt ->
      of_option opt enhance_volume)
    >>| List.filter_opt
  | a_bunch ->
    of_list_sequential a_bunch (fun id ->
      let query = Sql_query.get_volume_sexp id in
      Backend.query ~dbh query
      >>= fun r -> of_result (Sql_query.should_be_single r)
      >>= fun r -> of_result (Sql_query.parse_volume r)
      >>= enhance_volume)
  end
 

let generic_to_table type_info current_name r = []

let sortable_timestamp t =
  let open Html5 in
  let s = Timestamp.to_string t in
  `sortable (s,
             [ span ~a:[a_title s]
                 [pcdata 
                     (Time.(t |! to_local_date) |! Date.to_string)]])
    
let sortable_text s =
  let open Html5 in
  `sortable (s, [pcdataf "%s" s])
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
      :: sortable_text (Bool.to_string eval.f_recomputable)  (*  bool *)
      :: sortable_text (Float.to_string eval.f_recompute_penalty)  (*  float *)
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
    let ul_opt l = 
      ul (List.map (List.filter l ~f:(fun (g, f) -> g))
            ~f:(fun x -> li [snd x ()])) in
    let table_section prefix name table =
      let type_name = sprintf "%s_%s" prefix name in
      let is_only_one = List.length values = 1 in
      Authentication.authorizes (`edit `layout)
      >>= fun can_edit ->
      let editors_paragraph =
        if can_edit then
          content_paragraph [
            pcdataf "Actions: ";
            ul_opt [
              (can_edit, fun () ->
                self_link (`add_one_value type_name) (pcdataf "Add a %s" name));
              (can_edit && is_only_one, fun () -> 
                self_link (`edit_one_value (type_name, List.hd_exn values))
                  (pcdataf "Edit this %s" name));
            ]
          ]
        else
          content_list []
      in
      return (
        content_section (span [pcdataf "The %s " prefix; codef "%s" name])
          (content_list [
            editors_paragraph;
            content_table ~transpose:is_only_one table;
          ]))
    in
    of_list_sequential types ~f:(fun elt ->
      match find_node layout elt with
      | Some s -> 
        begin match s with
        | Enumeration (name, values) ->
          content_section (span [pcdata "Enumeration "; codef "%s" name])
            (content_paragraph 
               (List.map values (codef "`%s") |! interleave_list ~sep:(br ())))
          |! return

        | Record (name, typed_values) ->
          get_all_values ~only:values dbh name 
          >>| values_to_table typed_values
          >>= fun table ->
          table_section "record" name
            (typed_values_in_table (record_standard_fields @ ["S-Exp", String]
                                    @ typed_values) :: table)

        | Function (name, args, res)  ->
          get_all_evaluations ~only:values dbh name
          >>| evaluations_to_table args res
          >>= fun table ->
          let all_typed =
            function_standard_fields res @ ["S-Exp", String] @ args in
          table_section "function" name (typed_values_in_table all_typed :: table)
        | Volume (name, toplevel) ->
          get_all_volumes ~configuration ~only:values dbh name
          >>| volumes_to_table name toplevel
          >>= fun table ->
          let head s = List.map s (fun s -> `head [pcdataf "%s" s]) in
          table_section "volume" name
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

let raw_update ~configuration ~table ~g_id ~fields = return ()
  (*
  let query =
    sprintf "UPDATE %s SET %s WHERE g_id = %d" table
      (List.map fields (fun (n,_,v) -> 
        sprintf "%s = %s" n 
          (Option.value_map ~default:"NULL" ~f:(sprintf "'%s'") v))
        |! String.concat ~sep:", ")
      g_id
  in
  eprintf "QUERY: %s\n" query;
  with_database ~configuration 
    ~f:(pg_raw_query ~with_log:"web_raw_update" ~query)
  >>= fun _ -> return ()
  *)
  
let raw_insert  ~configuration ~table ~fields = return ()
  (*
  let query =
    sprintf "INSERT INTO %s (%s) VALUES (%s)" table
      (List.map fields (fun (n,_,_) -> n) |! String.concat ~sep:", ")
      (List.map fields (function
      | (_,_, None) -> "NULL"
      | (_,_, Some s) -> sprintf "'%s'" s) |! String.concat ~sep:", ")
  in
  eprintf "QUERY: %s\n" query;
  Hitscore_lwt.with_database ~configuration 
    ~f:(pg_raw_query ~with_log:"web_raw_insert" ~query)
  >>= fun _ -> return () *)

exception Edition_error of [ 
  `layout_edit_coservice_error of [
(*  | `fields_wrong_typing
  | `wrong_id
   | `wrong_rights *)
  | `io_exn of exn
  (*| `layout_inconsistency of [ `Record of string] *
      [ `insert_did_not_return_one_id of string * int32 list ]
  | `pg_exn of exn *)
  ]
]
    (*
let coservice_error e =
  Lwt.fail (Edition_error (`layout_edit_coservice_error e))

let one_time_post_coservice () =
  Eliom_output.Redirection.register_post_coservice
    ~scope:Eliom_common.session
    ~max_use:1
    ~fallback:Services.(home ())
    ~post_params:Eliom_parameters.(list "field" (string "str"))
    
let check_and_transform_input_value v t =
  (* WORK-IN-PROGRESS *)
  match v with
  | "" -> None
  | s ->
    begin match t with
    | LDSL.Array (LDSL.Enumeration_name enum) ->
      eprintf "it is an array: %s\n%!" s;
      Some ("("
            ^ String.concat ~sep:" " (String.split ~on:',' s)
            ^ ")")
    | _ -> Some s
    end
      
let editing_post_coservice ~configuration ~name ~value current_typed_values =
  (fun () fields ->
    let can_edit_m = Authentication.authorizes (`edit `layout) in
    Lwt.bind can_edit_m (fun can_edit ->
      match can_edit with
      | Ok true ->
        let m =
          let g_id = ref "" in
          try
            let n =
              List.map2_exn current_typed_values fields (fun (n,t,v1) v2 ->
                eprintf "replacing with value: %S\n%!" v2;
                let v = check_and_transform_input_value v2 t in
                if n = "g_id" then (
                  g_id := Option.value_exn v1; None
                ) else 
                  Some (n,t,v))
              |! List.filter_opt in
            Lwt.return (n, Int.of_string !g_id)
          with 
          | Invalid_argument s -> 
            coservice_error `fields_wrong_typing
          | Failure "Option.value_exn None" ->
            coservice_error `wrong_id
          | Failure s -> coservice_error `wrong_id
        in
        Lwt.bind m (fun (fields, g_id) ->
          Lwt.bind (raw_update ~configuration ~table:name ~fields ~g_id)
            (function
            | Ok () ->
              Lwt.return (Eliom_services.preapply  
                            Services.(layout ()) 
                            ("view", (["record_" ^ name], [value])))
            | Error e -> 
              coservice_error e))
      | _ ->
        coservice_error `wrong_rights))

let adding_post_coservice ~configuration ~name all_typed =
  (fun () fields ->
    let can_edit_m = Authentication.authorizes (`edit `layout) in
    Lwt.bind can_edit_m (fun can_edit ->
      match can_edit with
      | Ok true ->
        let m =
          try
            let n =
              List.map2_exn all_typed fields (fun (n,t) v2 ->
                eprintf "writing value: %S\n%!" v2;
                let v_transformed_back = check_and_transform_input_value v2 t in
                if n = "g_id" then None else Some (n,t,v_transformed_back))
              |! List.filter_opt in
            Lwt.return n
          with 
          | Invalid_argument s -> 
            coservice_error `fields_wrong_typing
        in
        Lwt.bind m (fun fields ->
          Lwt.bind (raw_insert ~configuration ~table:name ~fields)
            (function
            | Ok () ->
              Lwt.return (Eliom_services.preapply  
                            Services.(layout ())
                            ("view", (["record_" ^ name], [])))
            | Error e -> 
              coservice_error e))
      | _ ->
        coservice_error `wrong_rights))

let add_or_edit_one_record ~configuration ~name ~typed_values value_to_edit =
  let all_typed =  (LDSL.record_standard_fields @ typed_values) in
  Hitscore_lwt.with_database ~configuration ~f:(fun ~dbh ->
    of_option value_to_edit (fun one_value ->
      get_all_generic ~only:[one_value] dbh name
      >>= (function
      | [one_row] -> 
        begin 
          try
            return (one_value,
                    List.map2_exn one_row all_typed (fun v (n,t) -> (n,t,v)))
          with
            e -> error (`wrong_layout_typing name)
        end
      | not_one_row -> error (`did_not_get_one_row (name, not_one_row))))
    >>= fun currrent_typed_values ->
    let post_coservice =
      match currrent_typed_values with
      | Some (value, ctv) ->
        (one_time_post_coservice ()) 
          (editing_post_coservice ~configuration ~name ~value ctv)
      | None ->
        (one_time_post_coservice ())
          (adding_post_coservice ~configuration ~name all_typed)
    in
    let form =
      let form_title, typed_values =
        match currrent_typed_values with
        | Some (one_value, one) -> 
          (sprintf "Edit %s #%d" name one_value, one)
        | None ->
          (sprintf "Add a new %s" name,
           List.map all_typed (function
           | (n, LDSL.Option LDSL.Timestamp) -> 
             (n, LDSL.Option LDSL.Timestamp, Some Time.(now () |! to_string))
           | (n,t) -> (n,t, None))) in
      Eliom_output.Html5.(
        post_form ~service:post_coservice
          Html5.(fun values ->
            let f name (n, t, value) next =
              let msg = 
                sprintf "%s (%s)" n (LDSL.string_of_dsl_type t) in
              let styled_input = string_input ~a:[ a_style "min-width: 40em;"] in
              let selectable ?display n = 
                (Option ([], n, display, Some n = value)) in
              begin match t with
              | LDSL.Identifier ->
                [[ `head [pcdataf "Don't mess up with identifiers\n"];
                   `text [styled_input ~input_type:`Hidden ?value ~name ()];
                ]]
              | LDSL.Bool ->
                [[ `head [pcdata msg];
                   `text [string_select ~name
                             (selectable ~display:(pcdataf "False") "f") 
                             [selectable ~display:(pcdataf "True") "t"]]]]
              | LDSL.Enumeration_name enum ->
                begin match find_enumeration_values enum with
                | Some (h :: t) ->
                  [[ `head [pcdata msg];
                     `text [string_select ~name
                               (selectable h) (List.map t selectable)];]]
                | _ -> 
                  [[ `head [pcdata msg];
                     `text [styled_input ~input_type:`Text ~name ?value ()];]]
                end
              | LDSL.Array (LDSL.Enumeration_name enum) ->
                begin match find_enumeration_values enum with
                | Some values ->
                  let id = unique_id "input" in
                  Eliom_services.onload {{
                    Js.Opt.iter (Dom_html.document##getElementById (Js.string %id))
                    (fun input ->
                      let values_array = Array.of_list %values in
                      let basic_autocomplete =
                        jsnew Goog.Ui.AutoComplete.basic(
                          Js.array values_array,
                          input,
                          Js.some Js._true,
                          Js.some Js._true) in
                      ignore basic_autocomplete
                    )
                  }};
                  let value =
                    Option.map value ~f:(fun s ->
                      let a =
                        List.t_of_sexp String.t_of_sexp (Sexp.of_string s) in
                      String.concat ~sep:", " a) in
                  [[ `head [pcdata msg];
                     `text [string_input ~a:[ a_id id]
                               ~input_type:`Text ~name ?value ()];]]
                | _ -> 
                  [[ `head [pcdata msg];
                     `text [styled_input ~input_type:`Text ~name ?value ()];]]
                end
              | _ ->
                [[ `head [pcdata msg];
                   `text [styled_input ~input_type:`Text ~name ?value ()];]]
              end @ next
            in
            let submit =
              Eliom_output.Html5.string_input ~input_type:`Submit
                ~value:"Go" () in
            [
              h1 [pcdataf "%s" form_title];
              Template.(
                html_of_content
                  (content_table (values.Eliom_parameters.it f typed_values [])));
              div [submit]
            ])
          ())
    in
    return [form])

let edit_layout ~configuration ~main_title ~types ~values =
  match types, values with
  | [], _ ->
    error (`nothing_to_edit (types, values))
  | [one_type], values ->
    begin match values with
    | [] -> return None
    | [one] -> return (Some one)
    | _ -> error (`not_implemented "editing more than one value")
    end
    >>= fun  value_to_edit ->
    begin match find_node (Layout.Meta.layout ()) one_type with
    | Some (LDSL.Record (name, typed_values)) ->
      add_or_edit_one_record ~configuration ~name ~typed_values value_to_edit
    | _ -> error (`not_implemented "editing anything else than records")
    end
  | _, _ -> error (`not_implemented "editing more than one 'thing'")
    *)
    
let make ~configuration =
  (fun (action, (types, values)) () ->
    let main_title = "The Layout Navigaditor" in
    match action with
    | "view" ->
      let m =
        Authentication.authorizes (`view `layout)
        >>= function
        | true -> view_layout ~configuration ~main_title ~types ~values
        | false ->
          Template.make_authentication_error ~configuration ~main_title
            (return [Html5.pcdataf "You may not view the whole Layout."]) in
      Template.default ~title:main_title
        (bind_on_error m (fun e -> error (`Layout_service e)))
    | "edit" ->
      Template.default ~title:main_title (error (`not_implemented "reedit layout"))
        (*
      Template.default ~title:main_title
        (Authentication.authorizes (`edit `layout)
         >>= function
         | true -> edit_layout ~configuration ~main_title ~types ~values
         | false ->
           Template.make_authentication_error ~configuration ~main_title
             (return [Html5.pcdataf "You may not edit the Layout."])) *)
    | s ->
      Template.default ~title:main_title (error (`unknown_layout_action s)))
