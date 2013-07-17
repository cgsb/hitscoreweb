
open Hitscoreweb_std_server
{client{
open Hitscoreweb_std
}}
module Web_data_access = Hitscoreweb_data_access
module Authentication = Hitscoreweb_authentication
module Template = Hitscoreweb_template


let ls dir =
  let stream = Lwt_unix.files_of_directory dir in
  wrap_io Lwt_stream.to_list stream
  >>= fun files ->
  return files

let link_to_upload ~configuration ~and_remove (file, orig, date) =
  let open Html5 in
  let (post, get, remove) =
    Hitscoreweb_meta_form.Upload.init ~configuration () in
  (codef "[%s]" (Time.to_string date)) ::
    (Template.a_link (fun () -> get) [codef "%s" orig] file) ::
    (if and_remove then [
      pcdata " (";
      span ~a:[ a_class ["like_link"];
                a_onclick {{ fun _ ->
                  Lwt.ignore_result begin
                    let open Lwt in
                    let hu_host, hu_port = get_current_host_port () in
                    let url =
                      Printf.sprintf "https://%s:%d/%s/%s" hu_host hu_port
                        (String.concat "/"
                           Hitscoreweb_meta_form.Upload.remove_path)
                        (%file) in
                    XmlHttpRequest.perform_raw_url url
                    >>= fun result ->
                    dbg "XmlHttpRequest.perform_raw_url -> %S"
                      result.XmlHttpRequest.content;
                    Eliom_client.change_page
                      ~service:Eliom_service.void_hidden_coservice'
                      () ()
                  end
                           }}
              ] [pcdata "Remove"];
      pcdata ")";
     ] else
        [])

let make ~state =
  (fun () () ->
    let open Html5 in
    let configuration = state.Hitscoreweb_state.configuration in
    let content =
      Authentication.authorizes (`view `all_uploads)
      >>= begin function
      | true ->
        Authentication.spy_userf "Visit /uploads" >>= fun () ->
        let dir = Hitscoreweb_meta_form.Upload.uploads_dir configuration in
        ls dir
        >>| List.filter ~f:(fun f -> f <> "." && f <> "..")
        >>= fun all_files_in_uploads ->
        let check_list = ref all_files_in_uploads in
        let check (f, _, _) =
          let is_there = List.exists !check_list ((=) f) in
          check_list := List.filter !check_list ((<>) f);
          is_there
        in
        with_database configuration (fun ~dbh ->
          Access.Person.get_all ~dbh >>= fun all_persons ->
          while_sequential all_persons (fun p ->
            let person_id, gn, fn =
              let open Layout.Record_person in
              p.g_id, p.g_value.given_name, p.g_value.family_name in
            User_data.all_uploads ~dbh ~person_id
            >>= begin function
            | [] -> return []
            | some_uploads ->
              return [
                h2 [pcdataf "%d (%s, %s)" person_id fn gn];
                ul (List.map some_uploads ~f:(fun p ->
                  let is_there = check p in
                  let file, _, _ =  p in
                  li (link_to_upload ~configuration ~and_remove:true p @
                        (if is_there then []
                         else [
                           br ();
                           strong [pcdataf "But ";
                                   codef "%s/%s" dir file;
                                   pcdata " is missing!"]
                         ]))));
              ]
            end)
          >>| List.concat
          >>= fun sections ->
          let orphans =
            ul (List.map !check_list (fun p ->
              li [codef "%s" (Filename.concat dir p)])) in
          return [
            h1 [pcdata "People's Uploads"];
            div sections;
            h1 [pcdata "Orphan Uploads"];
            div [orphans];
          ])
      | false ->
        Authentication.spy_userf "Authentication error in /uploads" >>= fun () ->
        Template.make_authentication_error ~configuration ~main_title:"Uploads"
          (return [pcdataf "You may not view the Uploads."])
      end
    in
    Template.default ~title:"Uploads" content)
