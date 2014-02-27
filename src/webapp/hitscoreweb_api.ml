open Hitscoreweb_std_server

let authenticate ~configuration ?user ?token () =
  match user, token with
  | Some login, Some tok ->
    Hitscoreweb_data_access.find_person_opt login
    >>= fun person_opt ->
    begin match person_opt with
    |Some person ->
      with_database ~configuration (fun ~dbh ->
          let layout = Classy.make dbh in
          layout#person#get person#g_pointer
          >>= fun fresh_person ->
          while_sequential (Array.to_list fresh_person#auth_tokens) (fun t ->
              t#get
              >>= fun tok ->
              return (tok#hash)))
      >>= fun tokens ->
      dbg "tokens: %s" (String.concat ~sep:", " tokens);
      begin match List.mem tokens tok with
      | true ->
        return person
      | false ->
        error (`authentication "Nope")
      end
    | None ->
      error (`authentication "Nope")
    end
  | _, _ ->
    error (`authentication "Nope")

let list_libraries ~configuration ~user ~filter_qualified_names () =
  let find_fastq_files lib =
    let module Bui = Hitscore_interfaces.B2F_unaligned_information in
    while_sequential lib#submissions (fun sub ->
      while_sequential sub#flowcell#hiseq_raws (fun hr ->
        while_sequential hr#demultiplexings (fun dmux ->
          let read_number =
            Data_access.get_fastq_stats lib sub dmux
            |> Option.map ~f:(fun s -> Float.to_int s.Bui.cluster_count)
            |> Option.value ~default:0
          in
          map_option (Data_access.choose_delivery_for_user dmux sub)
            (fun (del, inv) ->
              let path =
                Option.(value_map ~default:"NO-DIR"
                          ~f:(fun d-> d#directory) del#client_fastqs_dir) in
              return path)
          >>= fun delivery_path ->
          begin match delivery_path with
          | None -> return None
          | Some del_path ->
            map_option dmux#unaligned (fun un ->
                let files =
                  Data_access.user_file_paths ~unaligned:un ~submission:sub lib in
                let hack_prefix path =
                  let prefix =
                    Option.value ~default:""
                      (Configuration.root_path configuration) in
                  String.chop_prefix ~prefix path
                  |>  Option.map ~f:(sprintf "/data/cgsb/gencore/%s")
                    (* this Hack is due to the fact that the webserver 
                       has not the same mount as the cluster which was
                       the initial plan *)
                in
                let r1s =
                  List.map files#fastq_r1s hack_prefix |> List.filter_opt in
                let r2s =
                  List.map files#fastq_r2s hack_prefix |> List.filter_opt in
                return (r1s, r2s, read_number)
              )
          end)
        >>| List.filter_opt))
    >>| List.concat
    >>| List.concat
  in
  Hitscoreweb_data_access.classy_cache ()
  >>= fun classy_cache ->
  dbg "qnames: %s" (String.concat ~sep:", " filter_qualified_names);
  let classy_libs = classy_cache#classy_libraries in
  let libraries =
    let persons_libraries =
      List.filter classy_libs#libraries
        (fun l ->
           let people =
             List.map l#submissions (fun sub -> sub#lane#contacts)
             |! List.concat in
             (* |! List.map ~f:(fun p -> p#g_t) in *)
           List.exists people (fun p -> user#g_id = p#g_id))
    in
    let pcre_matches rex str =
      try ignore (Pcre.exec ~rex str); true with _ -> false in
    let pcre_build qs =
      try Pcre.regexp ~flags:[`ANCHORED; `DOLLAR_ENDONLY] (qs ^ "$")
      with _ -> Pcre.regexp (String.make 42 'B') in
    if filter_qualified_names = []
    then persons_libraries
    else
      List.fold ~init:persons_libraries filter_qualified_names 
        ~f:(fun prev qs ->
            let rex = pcre_build qs in
            List.filter prev (fun l ->
                pcre_matches rex
                  (sprintf "%s.%s"
                     (Option.value ~default:"" l#stock#project) l#stock#name)))
  in
  while_sequential libraries (fun l ->
      find_fastq_files l >>= fun fastq_data ->
      let li_sample = Option.(l#sample >>= fun s -> return s#sample#name) in
      let li_organism =
        Option.(l#sample >>= fun s ->
                s#organism >>= fun o -> o#name) in
      let base = [
        l#stock#name;
        Option.value ~default:"" l#stock#project;
        Option.value ~default:"" l#stock#description;
        Option.value ~default:"" li_sample;
        Option.value ~default:"" li_organism;
      ] in
      let csv =
        List.map fastq_data (function
          | ([], _, c) -> base @ [ ""; ""; ""]
          | (r1 :: _, [], c) -> base @ [ r1; ""; Int.to_string  c]
          | (r1 :: _, r2 :: _, c) -> base @ [ r1; r2; Int.to_string c]
          )
      in
      (* return (object *)
      (*   method name = l#stock#name *)
      (*   method project = l#stock#project *)
      (*   method description = l#stock#description *)
      (*   method sample = li_sample *)
      (*   method organism = li_organism *)
      (*   method fastq_files = li_fastq_files *)
      (* end)) *)
      return csv)
  >>= fun result ->
  return (List.concat result)

let format_libraries ~format result =
  match format with
  | `CSV ->
    let res = ref [] in
    let csv_output_obj =
      Csv.to_out_obj ~separator:',' 
        (object
          method close_out () = ()
          method output s x y = 
            res := String.sub s x y :: !res;
            y 
        end) in
    Csv.(output_all csv_output_obj result);
    String.concat ~sep:"" (List.rev !res)

let run ~configuration ~query ?token ?user ~format ~filter_qualified_names () =
  authenticate ~configuration ?token ?user ()
  >>= fun user ->
  begin match query with
  | None | Some "libraries" ->
    begin match format with
    | Some "html/csv" -> return "text/html"
    | Some "csv" | None -> 
      (* returning `text/csv` because http://tools.ietf.org/html/rfc4180 *) 
      return "text/csv"
    | Some other -> error (`wrong_format other)
    end
    >>= fun return_type ->
    list_libraries ~user ~configuration ~filter_qualified_names ()
    >>= fun libs_csv ->
    return (format_libraries ~format:`CSV libs_csv, return_type)
  | Some other -> 
    error (`wrong_query other)
  end


let send_error fmt =
  ksprintf (fun s ->
      Eliom_registration.String.send ~code:404 (s, "text/html")) fmt

let service_api =
  make_delayed (Eliom_service.service
          ~path:["api"]
          ~get_params:Eliom_parameter.(
              opt (string "query") 
              ** opt (string "token")
              ** opt (string "user")
              ** opt (string "format")
              ** set string "filter_qualified_names"
            ))
let make ~configuration =
  Eliom_registration.Any.register
    ~service:(service_api ())
    Lwt.(fun (query, (token, (user, (format, filter_qualified_names)))) () ->
        dbg "query: %S user: %S token: %S format: %S filter_QN: [%s]" 
          (Option.value ~default:"not-provided" query)   
          (Option.value ~default:"not-provided" user)   
          (Option.value ~default:"not-provided" token)   
          (Option.value ~default:"not-provided" format)
          (String.concat ~sep:"," filter_qualified_names);
        run ~configuration ~query ?token ?user ~format
          ~filter_qualified_names ()
        >>= function
        | Ok (v, t) -> Eliom_registration.String.send (v, t)
        | Error (`wrong_query s) -> send_error "unknown query: %S" s
        | Error (`wrong_format s) -> send_error "unknown format: %S" s
        | Error (`authentication s) -> send_error "wrong credentials: %S" s
        | Error (`io_exn _)
        | Error (`Layout _)
        | Error (`db_backend_error _) -> send_error "server error"
      )




