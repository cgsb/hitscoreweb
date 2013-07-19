open Hitscoreweb_std_server
module Web_data_access = Hitscoreweb_data_access

(*

The goal of this module was to provide a less “global state” API to
the caches of information.

As of now, it uses `Web_data_access` to provide the `~state`
functions.

*)

type 'error global_errorful_state = {
  configuration: Configuration.local_configuration;
}

let init_state ~configuration
    ?(loop_waiting_time=45.)
    ?(allowed_age=60.)
    ?(maximal_age=900.)
    ()
    =
  {
    configuration;
  }

let configuration t = t.configuration
let persons_info t = Web_data_access.classy_cache ()

let find_person_opt ~state id =
  Web_data_access.find_person_opt id
let find_person ~state id =
  Web_data_access.find_person id

let person_by_pointer ~state p =
  Web_data_access.person_by_pointer p
