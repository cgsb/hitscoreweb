{shared{
open Hitscoreweb_std
}}
module Services = Hitscoreweb_services
module Authentication = Hitscoreweb_authentication
module Template = Hitscoreweb_template
module Persons_service = Hitscoreweb_persons

let gencore_users_stats layout =
  let open Template in
  let open Html5 in
  layout#lane#all >>= fun lanes ->
  layout#person#all
  >>| List.filter ~f:(fun p -> Array.exists p#roles ((=) `pi))
  >>= while_sequential ~f:(fun pi ->
    while_sequential (Array.to_list pi#affiliations) (fun aff_p ->
      aff_p#get)
    >>= fun affs ->
    let their_lanes =
      List.filter lanes (fun l ->
        Array.exists l#contacts (fun c -> c#id = pi#g_id)) in
    return (object
      method pi = pi
      method affiliations = affs
      method nb_of_lanes = List.length their_lanes
      method nb_of_libraries =
        List.fold_left their_lanes ~init:0 ~f:(fun s l ->
          s + (Array.length l#libraries))
    end))
  >>= fun informed_pis ->
  let table filter =
    let concerned =
      List.filter informed_pis (fun p ->
        List.exists p#affiliations (fun a -> filter a#path)) in
    let rows =
      List.map concerned (fun pi ->
        [ `text [pcdataf "%s" pi#pi#family_name];
          `text [pcdataf "%d" pi#nb_of_libraries];
          `text [pcdataf "%d" pi#nb_of_lanes]; ]) in
    content_table (
      [`head [pcdata "Name"]; `head [pcdata "# Libraries"]; `head [pcdata "# Lanes"]]
      :: rows
    )
  in
  return (content_section (pcdata "Gencore User Scores")
            (content_list [
              content_section (pcdata "CGSB")
                (table ((=) [| "NYU"; "Biology"; "CGSB" |]));
              content_section (pcdata "Bio. Dept.")
                (table ((=) [| "NYU"; "Biology" |]));
              content_section (pcdata "The Others")
                (table (fun a ->
                  a <> [| "NYU"; "Biology" |]
                  && a <> [| "NYU"; "Biology"; "CGSB" |]));
            ]))


let flowcell_data layout =
  layout#hiseq_run#all
  >>| List.sort ~cmp:(fun a b -> compare a#date b#date)
  >>= while_sequential ~f:(fun hsr ->
    let l = [hsr#flowcell_a; hsr#flowcell_b] |! List.filter_opt in
    while_sequential l (fun fc_p ->
      fc_p#get >>= fun fc ->
      while_sequential (Array.to_list fc#lanes) (fun l ->
        l#get >>= fun one_lane ->
        while_sequential (Array.to_list one_lane#contacts) (fun p ->
          p#get >>= fun person ->
          if Array.exists person#roles ((=) `pi)
          then return  (Some person)
          else return None)
        >>| List.filter_opt
        >>= fun pi_s ->
        let run_type =
          match one_lane#requested_read_length_1, one_lane#requested_read_length_2 with
          | n, None -> sprintf "SE %d" n
          | n, Some s -> sprintf "PE %dx%d" n s
        in
        return (pi_s, run_type, Array.length one_lane#libraries))
      >>= fun pi_runs ->
      return (object
        method run = hsr
        method fcid = fc#serial_name
        method run_type =
          List.nth pi_runs 1 |! Option.value_map ~default:"???" ~f:snd3
        method pi_s =
          List.map pi_runs (fun (pis, _, _) -> List.map pis (fun p -> p#family_name))
          |! List.concat
          |! List.dedup
        method nb_of_libraries = 
          List.fold_left (List.map pi_runs trd3)
            ~init:0 ~f:(fun a b -> a + b)
      end)))
  >>| List.concat

let mini_run_plan flowcells =
  let open Template in
  let open Html5 in
  let table =
    let rows =
      List.map flowcells (fun f ->
        [ `text [pcdata (Time.to_local_date f#run#date |! Date.to_string)];
          `text [pcdata f#fcid];
          `text [pcdata f#run_type];
          `text [pcdata String.(concat ~sep:", " f#pi_s)] ]) in
    content_table (
      [`head [pcdata "Run Date"]; `head [pcdata "FCID"];
       `head [pcdata "Run Type"]; `head [pcdata "P.I.(s)"] ]
      :: rows) in
  (content_section (pcdata "Mini-Run-Plan") table)
  

let libraries_per_month flowcells =
  let open Template in
  let open Html5 in
  let ym t =
    let d = Time.to_local_date t in
    (Date.year d, Date.month d) in
  let per_month =
    List.group flowcells ~break:(fun fa fb ->
      ym fa#run#date <> ym fb#run#date) in
  let table =
    let rows =
      List.map per_month (fun l ->
        let yr, mth = ym ((List.hd_exn l) #run#date) in
        let libraries =
          List.fold_left l ~init:0 ~f:(fun c f -> c + f#nb_of_libraries) in
        [ `text [pcdataf "%d, %s" yr (Month.to_string mth)];
          `text [pcdataf "%d" libraries] ]) in
    content_table ([ `head [pcdata "Month"]; `head [pcdata "# Libraries"] ] :: rows)
  in
  (content_section (pcdata "Libraries Per Month") table) 
    


{shared{
type up_message =
| Change_date of string * string option  (* 'g_id:field', newdate *)
deriving (Json)

type down_message = 
| Success
| Error_string of string

}}

let caml_service =
  make_delayed (Eliom_service.service 
          ~path:["stats_edition_caml_service"]
          ~get_params:Eliom_parameter.(caml "param" Json.t<up_message>))

let reply ~configuration = function
  | Change_date (id_field, newdate) ->
    Authentication.authorizes (`edit `facility_statistics)
    >>= fun authorization ->
    let errorf fmt = ksprintf (fun s -> error (`error_string s)) fmt in
    let date_time y m d =
      let y = Int.of_string y in
      let m = Int.of_string m |! Core.Month.of_int_exn in
      let d = Int.of_string d in
      Time.of_local_date_ofday
        (Date.create_exn ~y ~m ~d) (Core.Ofday.create  ~hr:10 ()) in 
    if authorization then 
      begin match String.split ~on:':' id_field with
      | [ g_id; field ] ->
        begin match try Some (Int.of_string g_id) with e -> None with
        | Some g ->
          let open Layout.Record_hiseq_statistics in
          with_database ~configuration (fun ~dbh ->
            Access.Hiseq_statistics.get ~dbh (unsafe_cast g)
            >>= fun hs ->
            begin match Option.map newdate (String.split ~on:'-') with
            | Some [yr; m; d] ->
              begin try 
                      return (Some (date_time yr m d))
                with e ->
                  errorf "Cannot parse date: %s"
                    (Option.value ~default:"None" newdate)
              end
            | None ->
              return None
            | _ ->
              errorf "Cannot parse date: %s"
                (Option.value ~default:"None" newdate)
            end
            >>= fun newtime ->
            begin match field with
            | "a_clustered" ->
              return { hs.g_value with a_clustered = newtime }
            | "a_started" -> 
              return { hs.g_value with a_started = newtime }
            | "a_finished"  ->
              return { hs.g_value with a_finished = newtime }
            | "a_returned"  -> 
              return { hs.g_value with a_returned = newtime }
            | "b_clustered" -> 
              return { hs.g_value with b_clustered = newtime }
            | "b_started"   ->  
              return { hs.g_value with b_started = newtime }
            | "b_finished"  ->  
              return { hs.g_value with b_finished = newtime }
            | "b_returned"  ->
              return { hs.g_value with b_returned = newtime }
            | _ ->
              errorf "Cannot parse: %s" id_field
            end
            >>= fun newvalue ->
            let newhs = { hs with g_value = newvalue } in
            Access.Hiseq_statistics.update ~dbh newhs
            >>= fun () ->
            return Success
          )
        | None ->
          errorf "Cannot parse: %s" id_field
        end
      | _ ->
        errorf "Cannot parse: %s" id_field
      end
    else
      errorf "Wrong Credentials !!!"
    
let init_caml_service ~configuration =
  let already = ref false in
  fun () ->
    if !already then () else (
      already := true;
      let fail fmt =
        ksprintf
          (fun s -> Lwt.fail (Failure ("wrong reply from server: " ^ s)))
          fmt
      in
      Eliom_registration.Ocaml.register ~service:(caml_service ())
        (fun param () ->
          Lwt.bind (reply ~configuration param) (function
          | Ok o -> Lwt.return (o : down_message)
          | Error (`error_string s) -> Lwt.return (Error_string s)
          | Error e -> fail "unknown error")))





    
{client{    
let rec popup_date_picker field_name button_id attach_id caml =
  dbg "Clicked %s (%s)" field_name button_id;
  let button = get_element_exn button_id in
  let attach = get_element_exn attach_id in
  dbg "Button: %s" (Js.to_string button##title);
  let date_picker = jsnew Goog.Ui.datePicker(Js.null, Js.null) in
  let remove_date_picker need_once =
    date_picker##dispose();
    let once = ref (not need_once) in
    button##onclick <- Dom_html.handler(fun _ ->
      dbg "button-onclick: once = %b" !once;
      if !once then (
        popup_date_picker field_name button_id attach_id caml;
      ) else (
        once := true;
      );
      Js._true);
  in
  button##onclick <- Dom_html.handler (fun _ ->
    dbg "removing date_picker";
    remove_date_picker false;
    Js._true);
  begin match Js.to_string button##title with
  | "None" ->
    date_picker##selectNone();
  | s ->
    begin match (Js.string s)##split (Js.string "-")
                |! Js.str_array |! Js.to_array with
    | [| yr; m; d |] ->
      let date = jsnew Js.date_day(int_of_string (Js.to_string yr),
                                   int_of_string (Js.to_string m) - 1,
                                   int_of_string (Js.to_string d)) in
      dbg "Date: %s --> %s" s (Js.to_string date##toString());
      date_picker##setDate(Goog.Tools.Union.i2 date);
    | l ->
      dbg "non split: %s" s;
      date_picker##selectNone();
    end
  end;
  date_picker##render(Js.some attach);
  Goog.Events.listen (Goog.Tools.Union.i1 date_picker) (Js.string "select")
    (Js.wrap_callback (fun _ ->
      dbg "callback there";
      Lwt.ignore_result (
        begin match Js.Opt.to_option (date_picker##getDate()) with
        | Some d ->
          let date = d##toIsoString(Js.some Js._true, Js.null) in
          button##title <- date;
          button##innerHTML <- date;
          dbg "Date: %s" (Js.to_string date);
          Eliom_client.call_caml_service ~service:caml
            (Change_date (field_name, Some (Js.to_string date))) ()
        | None ->
          button##title <- Js.string "None";
          button##innerHTML <- Js.string "None";
          dbg "No date";
          Eliom_client.call_caml_service ~service:caml
            (Change_date (field_name, None)) ()
        end
        >>= fun reply ->
        begin match reply with
        | Success ->
          button##style##background <- Js.string "#80D969";
        | Error_string err ->
          button##style##background <- Js.string "#D43223";
          button##style##color <- Js.string "#000000";
          button##innerHTML <- Printf.ksprintf Js.string "<b>Error: %s</b>" err
        end;
        dbg "disposing date_picker";
        remove_date_picker false;
        Lwt.return ()
     (* dbg "end of goog callback" *)
      )
     ))
    Js.null |! dbg "Listener: %d";
  ()
    
}}

let changeable_date name value =
  let open Template in
  let open Html5 in
  let str =
    Option.value_map value ~default:"None" ~f:(fun s ->
      Time.to_local_date s |! Date.to_string) in
  let button_id = unique_id "changeable_date" in
  let attach_id = unique_id "attach_date_picker" in 
  let caml = caml_service () in
  let span =
    Eliom_content.Html5.D.(
      div [
        div ~a:[ a_id button_id;
                  a_title str;
                  a_class ["like_link"];
                  a_onclick {{
                    popup_date_picker %name %button_id %attach_id %caml
                  }}]
          [pcdata str];
        div ~a:[ a_id attach_id ] []
      ])
  in
  (str, span)
    
let hiseq_stats layout =
  let open Template in
  let open Html5 in
  layout#hiseq_statistics#all
  >>= while_sequential ~f:(fun hs ->
    hs#run#get >>= fun hsr ->
    return (hs, hsr))
  >>| List.sort ~cmp:(fun (_, a) (_, b) -> compare a#date b#date)
  >>= fun hstats ->
  while_sequential hstats (fun (hs, hsr) ->
    let stat name s =
      let str, span = changeable_date (sprintf "%d:%s" hs#g_id name) s in
      `sortable (str, [span]) in
    let fc_row side stats p =
      p#get >>= fun fc ->
      let fc_link =
        a_link Services.flowcell [strong [pcdataf "%s" fc#serial_name]] fc#serial_name
      in
      let hr_date = Time.to_local_date hsr#date |! Date.to_string in
      [ `sortable (hr_date, [strongf "%s / %s" hr_date side]);
        `sortable (fc#serial_name, [fc_link])
      ] @ stats
      |! return in
    map_option hsr#flowcell_a (fun p ->
      fc_row "A" [
        stat "a_clustered" hs#a_clustered;
        stat "a_started"   hs#a_started;
        stat "a_finished"  hs#a_finished;
        stat "a_returned"  hs#a_returned; ] p)
    >>= fun a_row ->
    map_option hsr#flowcell_b (fun p ->
      fc_row "B" [
        stat "b_clustered" hs#b_clustered;
        stat "b_started"   hs#b_started;
        stat "b_finished"  hs#b_finished;
        stat "b_returned"  hs#b_returned; ] p)
    >>= fun b_row ->
    return (List.filter_opt [a_row; b_row]))
  >>| List.concat 
  >>= fun rows ->
  let head =
    [`head [pcdata "Hiseq-run"];
     `head [pcdata "Flowcell ID"];
     `head [pcdata "Date Clustered"];
     `head [pcdata "Date Started"];
     `head [pcdata "Date Finished"];
     `head [pcdata "Date Returned"]; ] in
  return (content_section (pcdata "Stats Draft") (content_table (head :: rows)))

    
let test_calendars () =
  let open Template in
  let open Html5 in
  Eliom_service.onload {{
    dbg "I'm there";
    let span = get_element_exn "label_iso_8601" in
    let date = ref None in
    let rec do_calendar () =
      let dp_iso_8601 = jsnew Goog.Ui.datePicker(Js.null, Js.null) in
      let attach = get_element_exn "attach_stats_table" in
      let sub = Html5.(div ~a:[ a_id "attach_calendar" ] []) |! Html5_to_dom.of_div in
      Dom.appendChild attach sub;
      span##onclick <- Dom_html.handler (fun _ -> Js._true);
      begin match !date with
      | Some d ->
        dp_iso_8601##setDate(Goog.Tools.Union.i1 d);
      | None ->
        dp_iso_8601##selectNone();
      end;
      dp_iso_8601##render(Js.some sub);
      Goog.Events.listen
        (Goog.Tools.Union.i1 dp_iso_8601)
        (Js.string "select")
        (Js.wrap_callback (fun _ ->
          dbg "callback there";
          begin match Js.Opt.to_option (dp_iso_8601##getDate()) with
          | Some d ->
            date := Some d;
            let s = Js.to_string d##toIsoString(Js.some Js._true, Js.null) in   
            dbg "Date: %s" s;
            span##innerHTML <- Printf.ksprintf Js.string "<b>Date: %s</b>" s;
          | None ->
            date := None;
            dbg "No date";
            span##innerHTML <- Printf.ksprintf Js.string "<b>No Date.</b>";
          end;
          dp_iso_8601##dispose();
          span##onclick <- Dom_html.handler(fun _ ->
            do_calendar ();
            Js._true);
         ))
        Js.null |! dbg "Listener: %d";
    in
    span##onclick <- Dom_html.handler(fun _ ->
      (* span##innerHTML <- Js.string "<b>Processing …</b>"; *)
      do_calendar ();
      Js._true);

    dbg "End";
  }};
  (content_section (pcdata "Test Calendar")
     (content_paragraph [
       Eliom_content.Html5.D.div ~a:[ a_id "attach_stats_table"] [];
       div ~a:[ a_id "attach_stats_table2"] [];
       span ~a:[ a_id "label_iso_8601"; a_style "clear:both" ] [pcdata "brout"];
     ]))

let statistics_page configuration =
  let open Template in
  let open Html5 in
  with_database configuration (fun ~dbh ->
    let layout = Classy.make dbh in
    gencore_users_stats layout
    >>= fun users_section ->
    flowcell_data layout >>= fun flowcells ->
    hiseq_stats layout >>= fun hstats ->
    return (content_list [users_section;
                          mini_run_plan flowcells;
                          libraries_per_month flowcells;
                          test_calendars ();
                          hstats]))

let make ~configuration =
  (fun () () ->
    let main_title = "Facility Statistics" in
    Template.default ~title:main_title
      (Authentication.authorizes (`view `facility_statistics)
       >>= function
       | true ->
         Template.make_content ~configuration ~main_title    
           (statistics_page configuration);
       | false ->
         Template.make_authentication_error ~configuration ~main_title
           (return [Html5.pcdataf 
                       "You may not view the facility statistics."])))
