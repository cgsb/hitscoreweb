open Printf

let () =
  let modules = List.rev (List.tl (Array.to_list Sys.argv)) in
  let rec go_through =
    function
    | [] -> ()
    | h :: t ->
      let deps =
        (String.concat " " (List.map (sprintf "_server/%s.cmi") t)) in
      printf "_server/%s.cmo: %s\n" h deps;
      printf "_server/%s.cmx: %s\n" h deps;
      printf "_client/%s.cmo: %s\n" h deps;
      printf "_client/%s.cmx: %s\n" h deps;
      go_through t
  in
  go_through modules
