open Printf

let () =
  let modules = List.rev (List.tl (Array.to_list Sys.argv)) in
  let rec go_through =
    function
    | [] -> ()
    | h :: t ->
      let deps where what =
        (String.concat " " (List.rev_map (fun s ->
          sprintf "_%s/%s.%s" where s what) t)) in
      printf "_server/%s.cmo: %s\n" h (deps "server" "cmo");
      printf "_server/%s.cmx: %s\n" h (deps "server" "cmx");   
      printf "_client/%s.cmo: %s\n" h (deps "client" "cmo");   
      printf "_client/%s.cmx: %s\n" h (deps "client" "cmx"); 
      printf "_client/%s.cmi: %s.type_mli\n" h h;  
      go_through t
  in
  go_through modules
