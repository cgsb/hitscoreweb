open Hitscoreweb_std

type 'error webapp_state = {
  configuration: Configuration.local_configuration; 
  persons_info:
    unit ->
    ('error Hitscore_data_access_types.classy_persons_information, 'error) t;
}

let init_state ~configuration
    ?(loop_waiting_time=5.)
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
  
let find_person_opt t id =
  persons_info t
  >>= fun classy_persons_info ->
  return (
    List.find_map classy_persons_info#persons (fun p ->
      if p#t#email = id || p#t#login = Some id ||
        Array.exists p#t#secondary_emails ((=) id)
      then Some p#t#g_t
      else None))
    
let find_person t id =
  find_person_opt t id
  >>= fun op ->
  begin match op with
  | Some s -> return s
  | None -> error (`person_not_found id)
  end
