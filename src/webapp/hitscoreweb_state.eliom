open Hitscoreweb_std

type 'error global_errorful_state = {
  configuration: Configuration.local_configuration; 
  persons_info:
    unit ->
    ('error Hitscore_data_access_types.classy_persons_information, 'error) t;
}

let init_state ~configuration
    ?(loop_waiting_time=25.)
    ?(allowed_age=60.)
    ?(maximal_age=900.)
    ()
    =
  let persons_classy_info =
    eprintf "state: Creation of classy persons\n%!";
    Data_access.init_classy_persons_information_loop
      ~loop_waiting_time ~log ~allowed_age ~maximal_age ~configuration
  in
  {
    configuration;
    persons_info  = persons_classy_info;
  }

let configuration t = t.configuration
let persons_info t = t.persons_info ()
  
let find_person_opt ~state id =
  persons_info state
  >>= fun classy_persons_info ->
  return (
    List.find_map classy_persons_info#persons (fun p ->
      if p#t#email = id || p#t#login = Some id ||
        Array.exists p#t#secondary_emails ((=) id)
      then Some p
      else None))
    
let find_person ~state id =
  find_person_opt ~state id
  >>= fun op ->
  begin match op with
  | Some s -> return s
  | None -> error (`person_not_found id)
  end

let person_by_pointer ~state p =
  persons_info state
  >>= fun classy_persons_info ->
  return (List.find classy_persons_info#persons (fun pc ->
    pc#t#g_pointer = p))
  >>= fun op ->
  begin match op with
  | Some s -> return s
  | None -> error (`person_not_found (sprintf "%d" p.Layout.Record_person.id))
  end

    
