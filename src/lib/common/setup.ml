let opam_download_cache =
  Obuilder_spec.Cache.v "download-cache"
    ~target:"/home/opam/.opam/download-cache"

let dune_build_cache =
  Obuilder_spec.Cache.v "dune-build-cache" ~target:"/home/opam/.cache/dune"

let network = [ "host" ]

let is_local commit =
  let repo = Current_git.Commit_id.repo commit in
  String.length repo > 0 && repo.[0] = '/'

let remote_uri commit =
  let repo = Current_git.Commit_id.repo commit in
  let commit = Current_git.Commit_id.hash commit in
  repo ^ "#" ^ commit

let remote_spec name commit =
  [ Obuilder_spec.run ~network "opam repo add %s %s" name (remote_uri commit) ]

let add_repositories repos =
  List.map
    (fun (name, commit) ->
      if is_local commit then [] else remote_spec name commit)
    repos
  |> List.flatten

let install_tools tools =
  let tools_s = String.concat " " tools in
  [
    Obuilder_spec.run ~network ~cache:[ opam_download_cache ]
      "opam install -y %s" tools_s;
  ]
