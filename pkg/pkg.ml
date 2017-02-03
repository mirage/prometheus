#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let includes = function
  | "prometheus" -> ["src"]
  | "prometheus-app" -> ["app"]
  | x -> failwith ("Unknown includes for package: " ^ x)

let build =
  let build_with_visible_warnings c os =
    let ocamlbuild = Conf.tool "ocamlbuild" os in
    let build_dir = Conf.build_dir c in
    let debug = Cmd.(on (Conf.debug c) (v "-tag" % "debug")) in
    let profile = Cmd.(on (Conf.profile c) (v "-tag" % "profile")) in
    let includes =
      match includes (Conf.pkg_name c) with
      | [] -> Cmd.empty
      | is -> Cmd.(v "-Is" % String.concat "," is)
    in
    Cmd.(ocamlbuild % "-use-ocamlfind" %% debug %% profile %% includes % "-build-dir" % build_dir)
  in
  let cmd c os files =
    OS.Cmd.run @@ Cmd.(build_with_visible_warnings c os %% of_list files)
  in
  Pkg.build ~cmd ()

let metas = [
  Pkg.meta_file ~install:false "pkg/META.prometheus";
  Pkg.meta_file ~install:false "pkg/META.prometheus-app";
]

let opams =
  let lint_deps_excluding = None in
  let install = false in
  [
    Pkg.opam_file "prometheus.opam" ~lint_deps_excluding ~install;
    Pkg.opam_file "prometheus-app.opam" ~lint_deps_excluding ~install;
  ]

let () =
  Pkg.describe ~opams ~metas ~build "prometheus" @@ fun c ->
  match Conf.pkg_name c with
  | "prometheus" -> Ok [
      Pkg.lib   "pkg/META.prometheus"   ~dst:"META";
      Pkg.lib   "prometheus.opam"       ~dst:"opam";
      Pkg.mllib "src/prometheus.mllib";
    ]
  | "prometheus-app" -> Ok [
      Pkg.lib   "pkg/META.prometheus-app" ~dst:"META";
      Pkg.lib   "prometheus-app.opam"     ~dst:"opam";
      Pkg.mllib "app/prometheus-app.mllib";
      Pkg.test  "tests/test" ~args:(Cmd.v "-q");
    ]
  | other -> R.error_msgf "unknown package name: %s" other
