{shared{
open Hitscoreweb_std
}}

module Queries = Hitscoreweb_queries

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template

module LDSL = Hitscoregen_layout_dsl

let read_file file =
  let io f = Lwt_io.(with_file ~mode:input f read) in
  wrap_io io file
    
let node_name = 
  let open LDSL in
  function
  | Enumeration (name, values) ->  "enumeration_" ^ name
  | Record (name, typed_values) -> "record_" ^ name
  | Function (name, args, res)  -> "function_" ^ name
  | Volume (name, toplevel) ->     "volume_" ^ name

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
  let name = node_name n in
  self_link (`view_one_type name) Html5.(codef "%s" name)

let find_node dsl nodename =
  let open LDSL in
  match String.lsplit2 nodename ~on:'_' with
  | None -> None
  | Some (prefix, suffix) ->
    List.find dsl.nodes (function
    | Enumeration (name,_) -> prefix = "enumeration" && name = suffix
    | Record (name, typed_values) -> prefix = "record"   && suffix = name
    | Function (name, args, res)  -> prefix = "function" && suffix = name
    | Volume (name, toplevel) ->     prefix = "volume"   && suffix = name
    )

let find_enumeration_values name =
  let open LDSL in
  List.find_map (Layout.Meta.layout ()).nodes 
    (function
    | Enumeration (enum, vals) when enum = name -> Some vals
    | _ -> None)


let typed_values_in_table l =
  let open Html5 in
  List.map l (fun (n, t) ->
    `head [
      b [codef "%s" n]; codef ": "; br ();
      i [codef "%s" (LDSL.string_of_dsl_type t)]]) 

let get_all_generic ?(only=[]) ?(more_where=[]) dbh name =
  let module PG = Layout.PGOCaml in
  let sanitize_name name =
    String.map name (function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' as c -> c
    | _ -> '_') in
  let sanitize_string s = String.map s (function ''' -> '_' | c -> c) in
  let query =
    let where = 
      match only, more_where with
      | [], [] -> ""
      | lo, lw ->
        sprintf " where (%s%s%s)"
          (List.map lo (sprintf "g_id = %d") |! String.concat ~sep:" or ")
          (if lw <> [] && lo <> [] then ") and (" else "")
          (List.map lw (fun (k, v) -> 
            sprintf "%s = '%s'" (sanitize_name k) (sanitize_string v))
            |! String.concat ~sep:" or ") 
    in
    sprintf  "select * from %s%s" (sanitize_name name) where in
  pg_raw_query ?with_log:None ~dbh ~query

let generic_to_table type_info current_name r =
  let open Html5 in
  let rec display_typed s  =
    let open LDSL in
    let text s = `sortable (s, [pcdataf "%s" s]) in
    let link s t =
      `sortable (s, [self_link (`view_one_value (t, Int.of_string s)) (pcdataf "%s" s)]) in
    function
    | Bool 
    | Int
    | Real
    | String -> text s
    | Timestamp -> 
      `sortable (s,
             [ span ~a:[a_title s]
                 [pcdata 
                     (Time.(of_string s |! to_local_date) |! Date.to_string)]])
    | Option o -> display_typed s o
    | Array (Record_name r) ->
      let ids = Array.to_list (Layout.PGOCaml.int32_array_of_string s)
                |! List.map ~f:Int32.to_int_exn in
      `sortable (List.map ids (sprintf "%d") |! String.concat ~sep:",",
        List.map ids (fun i -> 
          self_link (`view_one_value (sprintf "record_%s" r, i)) (pcdataf "%d" i))
          |! interleave_list ~sep:(pcdata ", "))
    | Array a -> text s
    | Record_name r -> link s (sprintf "record_%s" r)
    | Enumeration_name e -> text s
    | Function_name f -> link s (sprintf "function_%s" f)
    | Volume_name v -> link s (sprintf "volume_%s" v)
    | Identifier -> link s current_name
  in
  try
    List.map r ~f:(fun row ->
      List.map2_exn row type_info
        ~f:(fun sopt (n, t) ->
          Option.value_map sopt
            ~default:(`sortable ("",[codef "—"]))
            ~f:(fun s -> display_typed s t)))
  with
    e -> []

let volume_to_table name toplevel rows =
  let open Html5 in
  List.map rows ~f:(List.map ~f:(Option.value_map
                                   ~default:(`sortable ("", [codef "—"]))
                                   ~f:(fun s -> `sortable (s, [pcdataf "%s" s]))))


let view_layout ~configuration ~main_title ~types ~values =
  let content =
    Hitscore_lwt.db_connect configuration >>= fun dbh ->

    let layout = Layout.Meta.layout () in
    let nodes =
      Html5.(span (List.map layout.LDSL.nodes node_link
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
          let all_typed =  (record_standard_fields @ typed_values) in
          get_all_generic ~only:values dbh name 
          >>| generic_to_table all_typed (sprintf "record_%s" name)
          >>= fun table ->
          table_section "record" name (typed_values_in_table all_typed :: table)
        | Function (name, args, res)  ->
          let all_typed = function_standard_fields res @ args in
          get_all_generic ~only:values dbh name
          >>| generic_to_table all_typed (sprintf "function_%s" name)
          >>= fun table ->
          table_section "function" name (typed_values_in_table all_typed :: table)
        | Volume (name, toplevel) ->
          get_all_generic
            ~only:values ~more_where:["g_toplevel", toplevel] dbh "g_volume"
          >>| volume_to_table name toplevel
          >>= fun table ->
          let head s = List.map s (fun s -> `head [pcdataf "%s" s]) in
          table_section "volume" name
            (head ["Id"; "Top-level"; "Human-readable tag …"; "Files"] :: table)
        end
      | None -> 
        return (
          content_section (span [pcdata "Element "; codef "%s" elt])
            (content_paragraph
               [pcdataf "The element %S was not found" elt]))
    )
    >>= fun displayed_nodes ->
    Hitscore_lwt.db_disconnect configuration dbh >>= fun () -> 
    return (content_list (
      [content_section (pcdataf "The Layout") (content_paragraph [nodes]);] 
      @ displayed_nodes
    ))
  in
  Template.make_content ~configuration ~main_title content 

let raw_update ~configuration ~table ~g_id ~fields =
  let query =
    sprintf "UPDATE %s SET %s WHERE g_id = %d" table
      (List.map fields (fun (n,_,v) -> 
        sprintf "%s = %s" n 
          (Option.value_map ~default:"NULL" ~f:(sprintf "'%s'") v))
        |! String.concat ~sep:", ")
      g_id
  in
  eprintf "QUERY: %s\n" query;
  Hitscore_lwt.with_database ~configuration 
    ~f:(pg_raw_query ~with_log:"web_raw_update" ~query)
  >>= fun _ -> return ()

let raw_insert  ~configuration ~table ~fields =
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
  >>= fun _ -> return ()

exception Edition_error of [ 
  `layout_edit_coservice_error of [
  | `fields_wrong_typing
  | `wrong_id
  | `wrong_rights
  | `io_exn of exn
  | `layout_inconsistency of [ `record_log ] *
      [ `insert_did_not_return_one_id of string * int32 list ]
  | `pg_exn of exn
  ]
]
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
                  let dbg = debug_service () in
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
                      debugf %dbg "onload for %S -- after goog" %id;
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

let make ~configuration =
  (fun (action, (types, values)) () ->
    let main_title = "The Layout Navigaditor" in
    match action with
    | "view" ->
      Template.default ~title:main_title
        (Authentication.authorizes (`view `layout)
         >>= function
         | true -> view_layout ~configuration ~main_title ~types ~values
         | false ->
           Template.make_authentication_error ~configuration ~main_title
             (return [Html5.pcdataf "You may not view the whole Layout."]))
    | "edit" ->
      Template.default ~title:main_title
        (Authentication.authorizes (`edit `layout)
         >>= function
         | true -> edit_layout ~configuration ~main_title ~types ~values
         | false ->
           Template.make_authentication_error ~configuration ~main_title
             (return [Html5.pcdataf "You may not edit the Layout."]))
    | s ->
      Template.default ~title:main_title (error (`unknown_layout_action s)))
