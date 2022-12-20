open Core

module History = struct
  module App_id = struct
    let qualifier = ""
    let organization = ""
    let application = "swplayer"
  end

  module AppDirs = Directories.Project_dirs (App_id)

  let cache_directory = AppDirs.cache_dir |> Option.value_exn
  let history_file = Filename.concat cache_directory "history.txt"

  let add path =
    let _ = cache_directory in
    Stdio.Out_channel.with_file ~append:true ~binary:false
      ~f:(fun out -> Stdio.Out_channel.output_lines out [ path ])
      history_file

  (** Reads the history file. The resulting list contains the most recent entry first. *)
  let read () =
    try Stdio.In_channel.read_lines history_file |> List.rev
    with Sys_error _ -> []
end

let valid_extensions = [ "mp3"; "flac"; "wav"; "ogg" ]

(** [normalize_path path] normalizes the given [path] or raises an exception of failure. *)
let normalize_path path =
  path |> Fpath.of_string |> Result.ok |> Option.value_exn |> Fpath.normalize
  |> Fpath.to_string

(** [find_files ~extensions directory] collects all files in [directory] that have an extension in [~extensions] *)
let find_files ~extensions directory =
  let is_valid_extension ext = List.exists ~f:(String.equal ext) extensions in

  let check_and_normalize dir file =
    Filename.split_extension file
    |> snd
    |> Option.filter ~f:is_valid_extension
    |> Option.map ~f:(fun _ ->
           Filename.to_absolute_exn ~relative_to:dir file |> normalize_path)
  in

  let rec collect handle acc dir =
    match Core_unix.readdir_opt handle with
    | Some info ->
        let acc =
          check_and_normalize dir info
          |> Option.map ~f:(acc |> Fn.flip List.cons)
          |> Option.value ~default:acc
        in

        collect handle acc dir
    | None -> acc
  in

  try
    let handle = Core_unix.opendir directory in
    let res = collect handle [] directory in
    Core_unix.closedir handle;
    Some res
  with Core_unix.Unix_error _ -> None

let random_choice weights =
  let total = Array.reduce_exn ~f:( +. ) weights in
  let n = Array.length weights in
  let r = Random.float total in
  let rec find s pos =
    if pos >= n then n - 1
    else
      let s = s +. weights.(pos) in
      if Float.compare r s < 0 then pos else find s (pos + 1)
  in
  find 0. 0

(** Keeps only the first history entry for any file in [files]. *)
let filter_history ~files history =
  (* create table of all files *)
  let table = Hashtbl.create (module String) in
  files
  |> List.iter ~f:(fun file -> Hashtbl.add ~key:file ~data:() table |> ignore);

  history
  (* TODO: look in table whether [entry] exists, if it does, return Some entry and remove it from the table *)
  |> List.filter_map ~f:(fun entry ->
         Hashtbl.find_and_remove table entry |> Option.map ~f:(fun _ -> entry))

let run_player player file =
  let p = Core_unix.create_process ~prog:player ~args:[ file ] in
  Core_unix.waitpid p.pid

let play_single_file player files =
  let history = History.read () |> filter_history ~files in

  let never_played_files =
    List.filter
      ~f:(fun entry -> List.exists ~f:(String.equal entry) history |> not)
      files
  in

  let never_played_weights =
    Array.create ~len:(List.length never_played_files) 1.0
  in
  let history_weights =
    let n = List.length history in
    let denom_offset = if List.is_empty never_played_files then n else n + 1 in
    Array.init ~f:(fun i -> 1.0 /. float_of_int (denom_offset - i)) n
  in

  let weights = Array.concat [ never_played_weights; history_weights ] in

  let eligible_files =
    List.concat [ never_played_files; history ] |> List.to_array
  in

  let chosen_file =
    let i = random_choice weights in
    eligible_files.(i)
  in

  Stdio.printf "Playing: %s\n" chosen_file;
  Stdio.Out_channel.flush Stdio.stdout;
  History.add chosen_file;

  run_player player chosen_file |> Result.is_ok

let main player no_shutdown count directories =
  let files =
    directories
    |> List.map ~f:(fun path ->
           if Filename.is_relative path then
             Filename.to_absolute_exn ~relative_to:(Core_unix.getcwd ()) path
             |> normalize_path
           else path)
    |> List.map ~f:(find_files ~extensions:valid_extensions)
    |> List.filter_opt |> List.concat
  in

  if List.is_empty files |> not then
    let rec iter i =
      if i < count then
        match play_single_file player files with
        | true -> iter (i + 1)
        | _ -> false
      else true
    in

    if iter 0 && not no_shutdown then
      Core_unix.create_process ~prog:"shutdown" ~args:[ "-h"; "now" ] |> ignore

let () =
  let open Cmdliner in
  let count =
    let doc = "Play $(docv) files." in
    Arg.(value & opt int 1 & info [ "n"; "number" ] ~docv:"COUNT" ~doc)
  in

  let directories =
    let doc = "Each $(docv) is searched for audio files" in
    Arg.(value & pos_all dir [ "." ] & info [] ~docv:"DIRECTORY" ~doc)
  in

  let no_shutdown =
    let doc = "Do not perform shutdown" in
    Arg.(value & flag & info [ "S"; "no-shutdown" ] ~doc)
  in

  let player =
    let env =
      let doc = "Player executable name" in
      Cmd.Env.info "SWPLAYER_PLAYER" ~doc
    in
    let doc = "Player executable name" in
    Arg.(
      value & opt string "mpv" & info [ "p"; "player" ] ~env ~docv:"PLAYER" ~doc)
  in
  let term = Term.(const main $ player $ no_shutdown $ count $ directories) in
  let cmd = Cmd.v (Cmd.info "swplayer") term in

  exit (Cmd.eval cmd)
