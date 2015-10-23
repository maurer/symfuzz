(* SymFuzz *)

(** log printer

    @author Sang Kil Cha <sangkil.cha\@gmail.com>
    @since  2014-03-19

 *)

(*
Copyright (c) 2014, Sang Kil Cha
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL SANG KIL CHA BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*)

type out_color_t =
  | ColorRed
  | ColorGreen

module type LogInfo =
sig

  val chan : out_channel

end

module type Make =
  functor (L: LogInfo) ->
sig

  val init_log : string -> unit
  val fin_log : unit -> unit
  val set_debug_flag : bool -> unit
  val debugf : ('a, out_channel, unit) format -> unit
  val logf : ('a, out_channel, unit) format -> unit
  val logtimef : ('a, out_channel, unit) format -> unit
  val errf : out_color_t -> ('a, out_channel, unit) format -> unit
  val get_chan : unit -> out_channel

end

module Make(L: LogInfo) =
struct

  let debug_flag = ref false

  let chan = ref L.chan

  let logprint s =
    if !debug_flag then
      Printf.fprintf !chan "%s\n" s
    else
      ()

  let init_log file =
    let () = assert (!chan = L.chan) in
    chan := (open_out file)

  let fin_log () =
    if !chan <> L.chan then close_out !chan
    else ()

  let set_debug_flag flag = debug_flag := flag

  let time_print s =
    let t = Unix.localtime (Unix.time ()) in
    let s = Printf.sprintf "[%02d:%02d:%02d-%02d/%02d] %s"
            t.Unix.tm_hour
            t.Unix.tm_min
            t.Unix.tm_sec
            (t.Unix.tm_mon + 1)
            t.Unix.tm_mday
            s
    in
    Printf.sprintf "%s" s

  let time_log s =
    Printf.fprintf !chan "%s\n" s

  let debugf format =
    Printf.ksprintf (fun s -> logprint s) format
  let logf format =
    Printf.ksprintf (fun s -> Printf.fprintf !chan "%s\n" s) format
  let logtimef format =
    Printf.ksprintf (fun s -> let s = time_print s in time_log s) format

  let print_color chan color =
    let colorcode = match color with
      | ColorRed -> "\x1b[31m"
      | ColorGreen -> "\x1b[32m"
    in
    Printf.fprintf chan "%s" colorcode

  let clear_color chan =
    Printf.fprintf chan "\x1b[m"

  let errf color format =
    Printf.ksprintf (fun s ->
      print_color stderr color;
      prerr_string s;
      clear_color stderr) format

  let get_chan () = !chan

end

