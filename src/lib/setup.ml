let opam_download_cache =
  Obuilder_spec.Cache.v "download-cache" ~target:"/home/opam/.opam/download-cache"

let network = [ "host" ]

let remote_uri commit =
  let repo = Current_git.Commit_id.repo commit in
  let commit = Current_git.Commit_id.hash commit in
  repo ^ "#" ^ commit

let add_repositories =
  List.map (fun (name, commit) ->
      Obuilder_spec.run ~network "opam repo add %s %s" name (remote_uri commit))

let install_tools tools =
  let tools_s = String.concat " " tools in
  [ Obuilder_spec.run ~network ~cache:[ opam_download_cache ] "opam depext -i %s" tools_s ]
