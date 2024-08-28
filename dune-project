(lang dune 3.0)
(name arxiv_browser)
(generate_opam_files true)
(package
 (name arxiv_browser)
 (depends
  (ocaml (>= 5.2))
  (dune (>= 3.0))
  core
  core_unix
  async
  cohttp-async
  syndic
  notty
  notty-async
  ppx_jane))
