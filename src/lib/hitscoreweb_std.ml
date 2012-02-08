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
