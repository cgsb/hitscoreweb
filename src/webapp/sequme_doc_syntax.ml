open Core.Std

module Xml_tree = struct
  include Xmlm
  let in_tree i = 
    let el tag childs = `E (tag, childs)  in
    let data d = `D d in
    input_doc_tree ~el ~data i
end

type inline =
| Bold of inline list
| Italic of inline list
| Code of inline list 
| Link of [`url of string | `local of string] list * inline list
| Text of string

type structural =
| Section of [`one|`two|`three|`four] * string option * inline list
| Code_bloc of [`language of string] list * inline list
| Paragraph
| Line_break
| Inline of inline list
| Numbered_list of [`item of structural list] list
| Unnumbered_list of [`item of structural list] list
    
type document = structural list
    
open Result

type parsing_error = [
| `unknown_xml_tag of string
| `xml_syntax_error of Xml_tree.pos * Xml_tree.error
| `unexpected_text of string
]
exception Parsing_error of parsing_error

let parse ?(pedantic=true) file_content =
  begin try
          let content = "<content>" ^ file_content ^ "</content>" in
          return Xml_tree.(in_tree (make_input (`String (0, content))))
    with
    | Xml_tree.Error (p, e) -> fail (`xml_syntax_error (p, e))
  end
  >>= fun xml ->
  let continue l f = List.map l f |! List.concat in
  let find_attr name attrs =
    List.find_map attrs (fun ((_, attr), value) ->
      if name = attr then Some value else None) in
  let rec go_through_inline = function
    | `E (((_, "b"), _), inside) -> [Bold (continue inside go_through_inline)]
    | `E (((_, "i"), _), inside) -> [Italic (continue inside go_through_inline)]
    | `E (((_, "t"), _), inside) -> [Code (continue inside go_through_inline)]
    | `E (((_, "link"), attr), inside) ->
      let options = List.filter_opt [
        Option.map (find_attr "url" attr) (fun s -> `url s);
        Option.map (find_attr "local" attr) (fun s -> `local s);
      ] in
      [Link (options, continue inside go_through_inline)]
    | `E (((_, unknown), _), tl) ->
      if pedantic then
        raise (Parsing_error (`unknown_xml_tag unknown))
      else
        continue tl go_through_inline 
    | `D s -> [Text s]
  in
  let rec go_through_list = function
    | `E (((_,"item"), attrs), inside) -> [`item (continue inside go_through)]
    | `E (((_, unknown), _), tl) ->
      if pedantic then raise (Parsing_error (`unknown_xml_tag unknown))
      else continue tl go_through_list 
    | `D s ->
      if pedantic
        && String.exists s
          (function ' ' | '\t' | '\n' | '\r' -> false | _ -> true)
      then raise (Parsing_error (`unexpected_text s)) else []
  and go_through = function
    | `E (((_,"content"), _), inside) -> continue inside go_through
    | `E (((_, h_something), attrs), inside)
        when h_something = "h1" || h_something = "h2"
            || h_something = "h3" || h_something = "h4" ->
      let id = find_attr "id" attrs in
      let level =
        match String.(sub h_something 1 (length h_something - 1)) with
        | "1" -> `one | "2" -> `two | "3" -> `three | "4" -> `four
        | s -> raise (Parsing_error (`unknown_xml_tag h_something)) in
      [Section (level, id, continue inside go_through_inline)]
    | `E (((_,"p"), attrs), inside) -> [Paragraph]
    | `E (((_,"br"), attrs), inside) -> [Line_break]
    | `E (((_,"list"), attrs), inside) ->
      [Unnumbered_list (continue inside go_through_list)]
    | `E (((_,"enum"), attrs), inside) ->
      [Numbered_list (continue inside go_through_list)]
    | `E (((_,"code"), attrs), inside) ->
      let options = List.filter_opt [
        Option.map (find_attr "language" attrs) (fun s -> `language s) ] in
      [Code_bloc (options, continue inside go_through_inline)]
    | `E (t, tl) as e -> [Inline (go_through_inline e)]
    | `D s -> [Inline [Text s]]
  in
  try
    return (go_through (snd xml) : document)
  with
    Parsing_error e -> fail e

let extract_level_one document =
  let current_head = ref None in
  let current_doc = ref [] in
  let res = ref [] in
  List.iter document (function
  | Section (`one, id, content) ->
    Option.iter !current_head (fun (id, content) ->
      res := (id, content, List.rev !current_doc) :: !res;
    );
    current_doc := [];
    current_head := Some (id, content);
  | any_other ->
    current_doc := any_other :: !current_doc);
  Option.iter !current_head (fun (id, content) ->
    res := (id, content, List.rev !current_doc) :: !res;
  );
  List.rev !res
    


      
type table_of_contents_item =
| Toc of string * inline list * table_of_contents
and table_of_contents = table_of_contents_item list
    
let table_of_contents_raw (document: document) =
  let level_1 = ref [] in
  let current_1 = ref None in
  let level_2 = ref [] in
  let current_2 = ref None in
  let level_3 = ref [] in
  let current_3 = ref None in
  let level_4 = ref [] in
  let current_4 = ref None in
  let store_level_4 () =
    Option.iter !current_4 (fun (id, content) ->
      level_4 := Toc (id, content, []) :: !level_4;
    ); in
  let store_level_3 () =
    store_level_4 ();
    Option.iter !current_3 (fun (id, content) ->
      level_3 := Toc (id, content, List.rev !level_4) :: !level_3;
    );
    level_4 := [];
  in
  let store_level_2 () =
    store_level_3 ();
    Option.iter !current_2 (fun (id, content) ->
      level_2 := Toc (id, content, List.rev !level_3) :: !level_2;
    );
    level_3 := [];
    level_4 := [];
  in
  let store_level_1 () =
    store_level_2 ();
    Option.iter !current_1 (fun (id, content) ->
      level_1 := Toc (id, content, List.rev !level_2) :: !level_1;
    );
    level_2 := [];
    level_3 := [];
    level_4 := [];
  in
  let with_ids =
    let new_id =
      let count = ref 0 in
      fun () -> incr count; !count in
    List.map document (function
    | Section (level, id, content) ->
      let actual_id =
        match id with Some s -> s | None -> sprintf "section%d" (new_id ()) in
      begin match level with
      | `one ->
        store_level_1 ();
        current_1 := Some (actual_id, content);
        current_2 := None;
        current_3 := None;
        current_4 := None;
      | `two ->
        store_level_2 ();
        current_2 := Some (actual_id, content);
        current_3 := None;
        current_4 := None;
      | `three ->
        store_level_3 ();
        current_3 := Some (actual_id, content);
        current_4 := None;
      | `four ->
        store_level_4 ();
        current_4 := Some (actual_id, content);
      end;
      Section (level, Some actual_id, content)
    | e -> e)
  in
  store_level_1 ();
  (List.rev !level_1, with_ids)

let table_of_contents ?(toplevel:[`one|`two]=`one) (document: document) =
  match toplevel with
  | `one -> table_of_contents_raw document
  | `two ->
    let super = (Section (`one, None, []) :: document) in
    begin match table_of_contents_raw super with
    | (Toc (_, _, toc) :: _, doc) -> (toc, doc)
    | empty -> empty
    end

let rec toc_to_numbered_list toc =
  Numbered_list
    (List.map toc (function
    | Toc (id, content, subtoc) ->
      `item [Inline [Link ([`local id], content)];
             toc_to_numbered_list subtoc]))

    
let to_html ?(map_section_levels=ident) (v: document) =
  let buf = Buffer.create 42 in
  let str s = Buffer.add_string buf s in
  let chr c = Buffer.add_char buf c in
  let strf fmt = ksprintf (Buffer.add_string buf) fmt in 
  let rec inline = function
    | Bold inside -> 
      str "<b>"; List.iter inside inline; str "</b>"
    | Italic inside ->
      str "<i>"; List.iter inside inline; str "</i>"
    | Code  inside ->
      str "<code>"; List.iter inside inline; str "</code>"
    | Link (options, inside) ->
      strf "<a %s>"
        (List.map options (function
        | `url url -> sprintf "href=%S" url
        | `local id -> sprintf "href=\"#%s\"" id)
         |! String.concat ~sep:" ");
      List.iter inside inline;
      str "</a>"
    | Text s ->
      String.iter s (function
      | '<' -> str "&lt;"
      | '>' -> str "&gt;"
      | '&' -> str "&amp;"
      | c -> chr c)
  in
  let rec structural = function
    | Section (level, idopt, inside) ->
      let level_int =
        (match level with `one -> 1 | `two -> 2 | `three -> 3 | `four -> 4)
        |! map_section_levels
      in
      strf "</p><h%d%s>" level_int
        (Option.value_map idopt ~default:"" ~f:(sprintf " id=%S"));
      List.iter inside inline;
      strf "</h%d>\n<p>" level_int
    | Code_bloc (options, inside) ->
      str "</p>\n<pre>";
      List.iter inside inline;
      str "</pre><p>"
    | Paragraph -> str "</p><p>"
    | Line_break -> str "<br/>"
    | Inline i ->  List.iter i inline
    | Numbered_list items ->
      str "</p><ol>\n";
      List.iter items (function
      | `item ls -> str "<li><p>"; List.iter ls structural; str "</p></li>\n");
      str "</ol>\n<p>";
    | Unnumbered_list items ->
      str "</p><ul>\n";
      List.iter items (function
      | `item ls -> str "<li><p>"; List.iter ls structural; str "</p></li>\n");
      str "</ul>\n<p>";
  in
  str "<p>";
  List.iter v structural;
  str "</p>";
  Buffer.contents buf
