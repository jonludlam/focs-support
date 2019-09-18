let printer x =
  let filename,oc = Filename.open_temp_file "picture" ".ppm" in
  Printf.fprintf oc "%s" x;
  close_out oc;
  let pngfile = Filename.temp_file "picture" ".png" in
  ignore(Sys.command (Printf.sprintf "convert %s %s" filename pngfile));
  let ic = open_in pngfile in
  let n = in_channel_length ic in
  let data = really_input_string ic n in
  close_in ic;
  Sys.remove filename;
  Sys.remove pngfile;
  let base64=true in
  let mime="image/png" in
  ignore (Jupyter_notebook.display ~base64 mime data)
