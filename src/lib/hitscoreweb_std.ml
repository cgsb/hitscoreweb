include Core.Std
include Lwt

let (|>) x f = f x

module Html5 = Eliom_pervasives.HTML5.M

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module PGOCaml = PGOCaml_generic.Make(Lwt_thread)

(* Stupid force-linking test: *)
module TTT = Hitscore_std.Util

let _dummy_pg_test_ () =
  let host = "pg.bio.nyu.edu" in                                                                                  
  let user = "hitscore" in                                                                                        
  let password = "TODOFIX" in
  lwt dbh = PGOCaml.connect ~host ~user ~password ~port:5432 () in                                                
  (*  PGSQL(dbh) "execute" "create temporary table tblpeople                                                        
                          (name text not null, age int not null)";                                                
      let report (name,age) =                                                                                         
      Printf.printf "%s is %ld years old\n%!" name age in                                                           
      let results =                                                                                                   
      PGSQL (dbh) "select name, age from tblpeople" in                                                              
      List.iter report results                                                                                        
  *)                                                                                                                
  PGOCaml.ping dbh >>                                                                                             
  PGOCaml.close dbh   
