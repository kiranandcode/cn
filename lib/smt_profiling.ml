let out = ref (None : out_channel option)
let include_hash = ref false


let with_profiling ?(hash=fun () -> "") f v =
  match !out with
  | None -> f v
  | Some out ->
    let start_time = Unix.gettimeofday () in
    let res = f v in
    let time_elapsed = Unix.gettimeofday () -. start_time in
    let _ =
      if !include_hash
      then Printf.fprintf out "%s, %f\n" (hash ()) time_elapsed
      else Printf.fprintf out "%f\n" time_elapsed in
    res

let init_profiler (filename: string) =
  let rec make_unique_name_numeric (dir: string) (basename: string) (n: int) (ext: string) =
    let proposed_name = (basename ^ "_" ^ string_of_int n) ^ ext in
    if not (Sys.file_exists (Filename.concat dir proposed_name))
    then proposed_name
    else make_unique_name_numeric dir basename (n + 1) ext in
  let rec make_unique_name (dir: string) (dirname: string) (basename: string) (ext: string) =
    let proposed_name = Filename.concat dir basename ^ ext in
    if not (Sys.file_exists proposed_name)
    then proposed_name
    else if String.equal dirname Filename.current_dir_name
    then make_unique_name_numeric dir basename 1 ext
    else
      let basename = Filename.basename dirname ^ "_" ^ basename in
      let dirname = Filename.dirname dirname in
      make_unique_name dir dirname basename ext in
  Option.iter Out_channel.close !out;
  (match Sys.getenv_opt "CN_PROFILING_WITH_HASH" |> Option.map String.lowercase_ascii with
  | Some "true" -> include_hash := true
  | _ -> include_hash := false);
  match Sys.getenv_opt "CN_PROFILING_DIR" with
  | None ->
    out := None
  | Some dir ->
    if not (Sys.file_exists dir) then Sys.mkdir dir 0o700 else ();
    let basename = Filename.remove_extension (Filename.basename filename) in
    let dirname = Filename.dirname filename in
    let filename = make_unique_name dir dirname basename ".c.csv" in
    out := Some (open_out filename)

let close_profiler () =
  Option.iter Out_channel.close !out;
  out := None
