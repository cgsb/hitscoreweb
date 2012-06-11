

open Hitscoreweb_std

module Data_access = Hitscoreweb_data_access

module Queries = Hitscoreweb_queries

module Services = Hitscoreweb_services

module Authentication = Hitscoreweb_authentication

module Template = Hitscoreweb_template


let person_essentials dbh person_p =
  let layout = Classy.make dbh in
  layout#person#get person_p
  >>= fun p ->
  return (p#given_name, p#family_name, p#email)

let person_link ?(style=`full_name) dbh person_t =
  person_essentials dbh person_t >>= fun (f, l, e) ->
  let content =
    match style with
    | `full_name -> ksprintf Html5.pcdata "%s %s" f l
    | `family_name -> ksprintf Html5.pcdata "%s" l in
  return (Template.a_link Services.persons [content] (Some true, [e]))


let persons ~full_view ?(transpose=false) ?(highlight=[]) hsc =
  with_database hsc (fun ~dbh ->
    let layout = Classy.make dbh in
    layout#person#all >>= fun people ->
    while_sequential people (fun person ->
      let opt f m = Option.value_map ~f ~default:(f "") m in
      let is_vip = List.exists highlight ((=) person#email) in
      if not is_vip && (highlight <> []) then
        return None
      else Html5.(
        let email_field =
          let style = if is_vip then "color: green" else "" in
          `sortable (person#email, [code ~a:[a_id person#email; a_style style]
                                       [pcdata person#email]])
        in
        let text s = `sortable (s, [pcdata s]) in
        let opttext o = opt text o in
        let default = [
          opttext person#print_name;
          text person#given_name;
          opttext person#middle_name;
          text person#family_name;
          opttext person#nickname;
          email_field;
          `text (array_to_list_intermap person#secondary_emails ~sep:(pcdata ", ")
                   ~f:(codef "%s\n"));
          opttext person#login;
        ] in
        let supplement = 
          if not full_view then [] else [
            `text (array_to_list_intermap person#roles ~sep:(br ())
                     ~f:(fun s -> pcdataf "%s" 
                       (Layout.Enumeration_role.to_string s)));
            opttext person#note;]
        in
        return (Some (default @ supplement))))
    >>= fun rows ->
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

