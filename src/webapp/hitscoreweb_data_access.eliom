open Hitscoreweb_std
open Hitscore_lwt

let _global_broker : Hitscore_lwt.Broker.t option ref = ref None
let _global_timeout = ref 500.

let broker () =
  match !_global_broker with
  | Some b -> return b
  | None -> error `broker_not_initialized

let rec update  ~configuration () =
  with_database configuration (fun ~dbh ->
    begin match !_global_broker with
    | Some b ->
      eprintf "Reload Dump!\n%!";
      Broker.reload b ~dbh ~configuration 
    | None -> return ()
    end)
  >>= fun () ->
  wrap_io Lwt_unix.sleep !_global_timeout
  >>= fun () ->
  update ~configuration ()
  
let init ~loop_time ~configuration () =
  _global_timeout := loop_time;
  with_database configuration (fun ~dbh ->
    Broker.create ~dbh ~configuration ()
    >>= fun broker ->
    _global_broker := Some broker;
    return ())
  >>= fun () ->
  wrap_io Lwt_unix.sleep !_global_timeout
  >>= fun () ->
  update ~configuration ()


let find_person id =
  broker ()
  >>= fun broker ->
  Broker.find_person broker id
    
  
