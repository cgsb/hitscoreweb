
let () =
  Eliom_service.register_eliom_module
    "hitscoreweb"
    (fun () ->
      (* Eliom_registration.Html5.register *)
      let module Output_app =
            Eliom_registration.App (struct
              let application_name = "hitscoreweb"
            end) in
      Output_app.register
        ~service:(Eliom_service.service ~path:[""] ~get_params:Eliom_parameter.unit ())
        (fun () () ->
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
      

eliomc -noinfer hitscoreweb.eliom 
ocsigenserver -c bytecode.conf

    *)
