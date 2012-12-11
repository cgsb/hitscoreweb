open Printf
open Eliom_pervasives
{shared{
open Lwt
}}
let dbg fmt = ksprintf (eprintf "%s\n%!") fmt
let display_on_server =
  server_function Json.t<string>
    (fun x -> dbg "Client says %d bytes" (String.length x); Lwt.return (String.uppercase x))

{client{
let dbg fmt =
  Printf.ksprintf (fun s -> Firebug.console##debug(Js.string ("DBG: " ^ s))) fmt
}}
  
let () =
  dbg "Eliom_service.register_eliom_module";
  Eliom_service.register_eliom_module
    "hitscoreweb"
    (fun () ->
      (* Eliom_registration.Html5.register *)
      dbg "let module Output_app =";
      let module Output_app =
            Eliom_registration.App (struct
              let application_name = "hitscoreweb"
              let () =
                dbg "function application"
            end) in
      Output_app.register
        ~service:(Eliom_service.service ~path:[""] ~get_params:Eliom_parameter.unit ())
        (fun () () ->
          dbg "(fun () () ->";
          ignore {unit{
            dbg "onload!";
            Lwt.ignore_result begin
              Lwt_list.iter_s (fun n ->
                dbg "send %d bytes" n;
                %display_on_server String.(make n 'B');
                >>= fun s ->
                dbg "Got back %d bytes" (String.length s);
                Lwt_js.sleep 1.
              ) [100_000; 1_000_000; 2_000_000; 5_000_000; ]
              >>= fun () ->
              dbg "done";
              return ()
            end
          }};
          Lwt.return
            Eliom_content.Html5.D.(
              html
                (head (title (pcdata "TITLE")) [])
                (body [
                  div [pcdata "Hello!"]
                ])))
    )

    (*
eliomopt -c hitscoreweb.eliom 
ocamlfind ocamlopt -linkall -package ocsigenserver,ocsigenserver.ext.ocsipersist-sqlite -package eliom.server,ocsigenserver.ext.staticmod  _server/hitscoreweb.cmx server_main.cmx -o ttttt -linkpkg -thread
./ttttt -c static.conf
wget localhost:8080
      

eliomc -infer hitscoreweb.eliom && eliomc -c hitscoreweb.eliom && js_of_eliom hitscoreweb.eliom -o _client/hitscoreweb.js
ocsigenserver -c bytecode.conf

    *)
