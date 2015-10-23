(* Abstract Interpretation-based Fuzz Testing *)

open Libinput

let toolroot = try Sys.getenv "AFUZZ_PATH" with Not_found -> ".."

let pin_path =
  let pinpath = Filename.concat toolroot "pin/pin" in
  if Sys.file_exists pinpath then pinpath
  else failwith "pin is not found. try to set AFUZZ_PATH environment variable."

let instrumentor_path =
  let path =
    if Nativeint.size = 64 then
      Filename.concat toolroot "src/instrumentor/obj-intel64/symfuzz.so"
    else
      Filename.concat toolroot "src/instrumentor/obj-ia32/symfuzz.so"
  in
  if Sys.file_exists path then path
  else failwith "instrumentor is not found."

let execute vector logdir id sockname filenames debug_flag binname =
  let cmdlines = get_commandline vector in
  let instrumentor_logpath = Filename.concat logdir "instrument.log" in
  let filenames =
    List.fold_left (fun acc path -> "-filename"::path::acc) [] filenames
  in
  let binname = match binname with
    | Some name -> ["-binname"; name]
    | None -> []
  in
  let cmd =
    [
      pin_path;
      (* "-pause_tool"; "10"; *)
      "-t"; instrumentor_path;
      "-d"; logdir;
      "-o"; instrumentor_logpath;
      "-id"; string_of_int id;
      "-sock"; sockname;
    ]
    @ binname
    @ filenames
    @ (if debug_flag then ["-debug"] else []) @
    [
      "--";
    ] @ cmdlines
  in
  let cmdstring = String.concat " " cmd in
  let () = Printf.printf "cmd = (%s)\n" cmdstring in
  let () = flush stdout in
  let () = Unix.putenv "LD_LIBRARY_PATH" "_build" in (* TODO *)
  let _ = Process.fork_and_exec cmd in
  ()

