type 'a t = Lf | Br of 'a * 'a t * 'a t

let printer (str_of_alpha : 'a -> string) (x : 'a t) =
  let filename,oc = Filename.open_temp_file "tree" ".dot" in
  Printf.fprintf oc "digraph D {\n";
  let rec dump_nodes n = function
    | Lf -> Printf.fprintf oc "  Lf%d [label=\"Lf\"]\n" n
    | Br (x,t1,t2) ->
      Printf.fprintf oc "  Br%d [label=%s]\n" n (str_of_alpha x);
        dump_nodes (2*n+1) t1; 
        dump_nodes (2*n+2) t2
  in
  dump_nodes 0 x;
  let dump_edges t =
    let rec inner n1 n2 = function
    | Lf -> Printf.fprintf oc "  Br%d -> Lf%d\n" n1 n2
    | Br (_,t1,t2) ->
        Printf.fprintf oc "  Br%d -> Br%d\n" n1 n2;
        inner n2 (2*n2+1) t1;
        inner n2 (2*n2+2) t2
    in
    match t with
    | Lf -> ()
    | Br (_,t1,t2) -> inner 0 1 t1; inner 0 2 t2
  in
  dump_edges x;
  Printf.fprintf oc "}\n";
  close_out oc;
  let svgfile = Filename.temp_file "tree" ".svg" in
  let cmd = "dot -Tsvg -o" ^ svgfile ^ " " ^ filename in
  let _status = Sys.command cmd in
  let ic = open_in svgfile in
  let n = in_channel_length ic in
  let data = really_input_string ic n in
  close_in ic;
  let base64=false in
  let mime="image/svg+xml" in
  ignore (Jupyter_notebook.display ~base64 mime data);
  let rec to_text = function
    | Lf -> "Lf"
    | Br(x,l,r) -> Printf.sprintf "Br(%s,%s,%s)" (str_of_alpha x) (to_text l) (to_text r)
  in
  let text = to_text x in
  ignore (Jupyter_notebook.display "text/plain" text)


