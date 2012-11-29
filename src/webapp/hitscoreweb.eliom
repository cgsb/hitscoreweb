open Printf
let dbg fmt = ksprintf (eprintf "%s\n%!") fmt
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
          Lwt.return
            Eliom_content.Html5.D.(
              html
                (head (title (pcdata "TITLE")) [])
                (body [
                  div [pcdata "Hello!"]
                ])))
    )

    (*
eliomopt -noinfer hitscoreweb.eliom 
ocamlfind ocamlopt -linkall -package ocsigenserver,ocsigenserver.ext.ocsipersist-sqlite -package eliom.server,ocsigenserver.ext.staticmod  _server/hitscoreweb.cmx server_main.cmx -o ttttt -linkpkg -thread
./ttttt -c static.conf
wget localhost:8080
      

eliomc -noinfer hitscoreweb.eliom 
ocsigenserver -c bytecode.conf

    *)
