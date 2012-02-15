include Core.Std
(* include Lwt *)

let (|>) x f = f x

module Html5 = struct
  include Eliom_pervasives.HTML5.M
    
  let pcdataf fmt = ksprintf pcdata fmt

  let codef fmt = ksprintf (fun s -> code [pcdata s]) fmt

end


module Lwt_config = struct
  include Lwt
  include Lwt_chan
  let map_sequential l ~f = Lwt_list.map_s f l
  let log_error s = output_string Lwt_io.stderr s >>= fun () -> flush Lwt_io.stderr
    
  exception System_command_error of Lwt_unix.process_status
  let system_command s = 
    Lwt_unix.(
      system s >>= function
      | WEXITED 0 -> return ()
      | e -> fail (System_command_error e))

  let write_string_to_file s f =
    Lwt_io.(
      with_file ~mode:output s (fun o ->
        output_string o s))

end
module Hitscore_lwt = Hitscore.Make(Lwt_config)
module Layout = Hitscore_lwt.Layout
module PGOCaml = Layout.PGOCaml
module Configuration = Hitscore_lwt.Configuration

include Hitscore_lwt.Result_IO


let rec interleave_list ~sep = function
  | [] -> []
  | [one] -> [one]
  | l :: t -> l :: sep :: interleave_list ~sep t

let array_to_list_intermap ~sep ~f a =
  interleave_list ~sep (List.map (Array.to_list a) ~f)

let pg_raw_query ~dbh ~query =
  let module PG = Layout.PGOCaml in
  let name = "todo_change_this" in
  wrap_io (PG.prepare ~name ~query dbh) ()
  >>= fun () ->
  wrap_io (PG.execute ~name ~params:[] dbh) ()
  >>= fun result ->
  wrap_io (PG.close_statement dbh ~name) ()
  >>= fun () ->
  return result
