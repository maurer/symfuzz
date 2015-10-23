(* SymFuzz *)

(** myocamlbuild

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

open Ocamlbuild_plugin
open Ocamlbuild_pack

let run_and_read = Ocamlbuild_pack.My_unix.run_and_read

let split s ch =
  let x = ref [] in
  let rec go s =
    let pos = String.index s ch in
    x := (String.before s pos)::!x;
    go (String.after s (pos + 1))
  in
  try
    go s
  with Not_found -> !x

let split_nl s = split s '\n'

let before_space s =
  try String.before s (String.index s ' ')
  with Not_found -> s

(* this lists all supported packages *)
let find_packages () =
  List.map before_space (split_nl & run_and_read "ocamlfind list")

(* ocamlfind command *)
let ocamlfind x = S[A"ocamlfind"; x]

(* camlidl command *)
let camlidl = S([A"camlidl"])

let getline_from_cmd cmd =
  let ch = Unix.open_process_in cmd in
  let line = input_line ch in
  ignore (Unix.close_process_in ch);
  line

(* ocaml path *)
let ocamlpath = getline_from_cmd "ocamlfind printconf path"

(* read clib file and make P *)
let read_clib file =
  let chan = open_in file in
  let specs = ref [] in
  try
    while true do specs := P(input_line chan) :: !specs done; []
  with End_of_file ->
    close_in chan; List.rev !specs

let _ = dispatch begin function
  | Before_options ->

      (* override default commands by ocamlfind ones *)
      Options.ocamlc     := ocamlfind & A"ocamlc";
      Options.ocamlopt   := ocamlfind & A"ocamlopt";
      Options.ocamldep   := ocamlfind & A"ocamldep";
      Options.ocamldoc   := ocamlfind & A"ocamldoc";
      Options.ocamlmktop := ocamlfind & A"ocamlmktop";

      (* packages taggings *)
      tag_any
        ["pkg_str";
         "pkg_unix";
         "pkg_batteries";
         "pkg_threads";
         "pkg_yojson";
         "pkg_camlidl";
         "pkg_libbil";
         "pkg_libinput";
        ];

      tag_file "analyzer/pinapi_stubs.c" ["stubs"];

  | After_rules ->

      flag ["ocaml"; "link"; "program"] & A"-linkpkg";

      (* For each ocamlfind package one inject the -package option when
       * compiling, computing dependencies, generating documentation and
       * linking. *)
      List.iter begin fun pkg ->
        flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
      end (find_packages ());

      (* The default "thread" tag is not compatible with ocamlfind.
         Indeed, the default rules add the "threads.cma" or "threads.cmxa"
         options when using this tag. When using the "-linkpkg" option with
         ocamlfind, this module will then be added twice on the command line.

         To solve this, one approach is to add the "-thread" option when using
         the "threads" package using the previous plugin.
       *)
      flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
      flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
      flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"]);

      (* debugging and optimization *)
      flag ["ocaml"; "compile"] (S[A"-g"]);
      flag ["ocaml"; "link"] (S[A"-g"]);
      flag ["ocaml"; "compile"; "native"] (S[A"-inline";A"10"]);

      (* c stub generated from camlidl *)
      flag ["c"; "compile"; "stubs"]
        (S[A"-ccopt";A("-I"^ocamlpath^"/camlidl");]);

      flag ["cpp"; "compile"; "stubs"]
        (S[A("-I"^ocamlpath^"/ocaml");A("-I"^ocamlpath^"/camlidl");]);

      flag ["c"; "compile"; "file:analyzer/pinapi_stubs.c"]
        (S[A"-ccopt"; A"-I../pinapi"]);

      flag ["ocaml"; "link"; "native"]
        (S[
          A"-inline"; A"10";
          A"-cclib"; A"-L.";
          A"-cclib"; A"-lboost_system";
          A"-cclib"; A"-lboost_filesystem";
          A"-cclib"; A"-lcamlidl";
          A"-cclib"; A"-lstdc++";
        ]);

      flag ["ocamlmklib"; "c"] (S[A"-L."]);

      (* camlidl rules starts here *)
      rule "camlidl"
        ~prods:["%.mli"; "%.ml"; "%_stubs.c"]
        ~deps:["%.idl"]
        begin fun env _build ->
          let idl = env "%.idl" in
          let tags = tags_of_pathname idl ++ "compile" ++ "camlidl" in
          let cmd = Cmd( S[camlidl; T tags; P idl] ) in
          Seq [cmd]
        end;

      (* define c++ ruels here *)
      rule "cpp"
        ~prods:["%.o";]
        ~deps:["%.cpp"]
        begin fun env _build ->
          let file = env "%.cpp" in
          let target = env "%.o" in
          let tags = tags_of_pathname file ++ "compile" ++ "cpp" in
          let cmd = Cmd( S[A"g++"; A"-g"; A"-c"; A"-O3";
                           A("-I../"^Pathname.dirname file);
                           A("-I/usr/local/include");
                           A("-fPIC");
                           A"-o"; A target; T tags; P file] )
          in
          Seq [cmd]
        end;

      (* specific rule for libanalysis *)
      rule "libanalysis"
        ~prods:["libanalysis.native";]
        ~deps:["analyzer/pinapi_stubs.o";
               "analyzer/analysis.cmx";
               "analyzer/rcfg.cmx"]
        begin fun env _build ->
          let file = env "libanalysis.so" in
          let tags =
            tags_of_pathname file ++ "ocaml" ++ "link" ++ "native"
          in
          let cmd = Cmd( S[!Options.ocamlopt;
                           A"-linkpkg";
                           S(read_clib (env "../libanalysis.clib"));
                           A"-output-obj";
                           A"-o"; A file;
                           T tags;] )
          in
          Seq [cmd]
        end

  | _ -> ()
end

