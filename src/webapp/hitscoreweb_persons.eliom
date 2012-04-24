

open Hitscoreweb_std

module Data_access = Hitscoreweb_data_access

module Queries = Hitscoreweb_queries

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template


let person_essentials dbh person_t =
  Layout.Record_person.(
    get ~dbh person_t
    >>= fun { given_name; family_name; email; _ } ->
    return (given_name, family_name, email))

let person_link ?(style=`full_name) dbh person_t =
  person_essentials dbh person_t >>= fun (f, l, e) ->
  let content =
    match style with
    | `full_name -> ksprintf Html5.pcdata "%s %s" f l
    | `family_name -> ksprintf Html5.pcdata "%s" l in
  return (Template.a_link Services.persons [content] (Some true, [e]))


let persons ~full_view ?(transpose=false) ?(highlight=[]) hsc =
  Hitscore_lwt.db_connect hsc
  >>= fun dbh ->
  Layout.Record_person.(
    get_all ~dbh >>= fun plist ->
    let rows_m =
      of_list_sequential plist (fun p ->
        get ~dbh p
        >>= fun {print_name; given_name; middle_name; family_name;
  	         email; secondary_emails; roles; login; nickname; note;} ->
        let opt f m = Option.value_map ~f ~default:(f "") m in
        let is_vip = List.exists highlight ((=) email) in
        if not is_vip && (highlight <> []) then
          return None
        else Html5.(
          let email_field =
            let style = if is_vip then "color: green" else "" in
            `sortable (email, [code ~a:[a_id email; a_style style]
                                  [pcdata email]])
          in
          let text s = `sortable (s, [pcdata s]) in
          let opttext o = opt text o in
          let default = [
            opttext print_name;
            text given_name;
            opttext middle_name;
            text family_name;
            opttext nickname;
            email_field;
            `text (array_to_list_intermap secondary_emails ~sep:(pcdata ", ")
                     ~f:(codef "%s\n"));
            opttext login;
          ] in
          let supplement = 
            if not full_view then [] else [
              `text (array_to_list_intermap roles ~sep:(br ())
                       ~f:(fun s -> pcdataf "%s" 
                         (Layout.Enumeration_role.to_string s)));
              opttext note;]
          in
          return (Some (default @ supplement)))) in
    rows_m >>= fun rows ->
    Hitscore_lwt.db_disconnect hsc dbh
    >>= fun _ ->
    let actual_rows = List.filter_opt rows in
    let nrows = List.length actual_rows in
    return Template.(Html5.(
      let normal_rows = [
        `head [pcdata "Print name"];
	`head [pcdata "Given name"];
	`head [pcdata "Middle name"];
	`head [pcdata "Family name"];
	`head [pcdata "Nickname"];
	`head [pcdata "Email"];
        `head [pcdata "Secondary Emails"];
	`head [pcdata "Login"]] in
      let supplement = 
        if not full_view then [] else [
	  `head [pcdata "Roles"];
	  `head [pcdata "Note"];] in
      content_section 
        (ksprintf pcdata "Found %d Person%s" nrows
           (if nrows > 1 then "s" else ""))
        (content_table ~transpose
           ((normal_rows @ supplement)
            :: actual_rows)))))

let make configuration =
  (fun (transpose, highlight) () ->
    Template.default ~title:"Persons"
      (Authentication.authorizes (`view `persons)
       >>= function
       | true ->
         Authentication.authorizes (`view `full_persons)
         >>= fun full_view ->
         Template.make_content ~configuration
           ~main_title:"People" 
           (persons ?transpose ~highlight ~full_view configuration)
       | false ->
         Template.make_authentication_error ~configuration
           ~main_title:"Persons" 
           (return [Html5.pcdataf "You may not view any person."])))

