type t = Lf | Br of int * t * t

let printer (x : t) =
  let filename,oc = Filename.open_temp_file "tree" ".dot" in
  Printf.fprintf oc "digraph D {\n";
  let rec dump_nodes = function
    | Lf -> ()
    | Br (x,t1,t2) ->
        Printf.fprintf oc "  %d\n" x;
        dump_nodes t1; 
        dump_nodes t2
  in
  dump_nodes x;
  let dump_edges t =
    let rec inner from = function
    | Lf -> ()
    | Br (x,t1,t2) ->
        Printf.fprintf oc "  %d -> %d\n" from x;
        inner x t1;
        inner x t2
    in
    match t with
    | Lf -> ()
    | Br (x,t1,t2) -> inner x t1; inner x t2
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
  ignore (Jupyter_notebook.display ~base64 mime data)

