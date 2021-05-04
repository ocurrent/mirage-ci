let profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "dev" | None -> `Dev
  | Some "docker" -> `Docker
  | Some x -> Fmt.failwith "Unknown $PROFILE setting %S" x

let to_obuilder_job build_spec =
  let open Current.Syntax in
  let+ build_spec = build_spec in
  let spec_str = Fmt.to_to_string Obuilder_spec.pp (build_spec |> Spec.finish) in
  let open Cluster_api.Obuilder_job.Spec in
  { spec = `Contents spec_str }

let to_docker_job build_spec =
  let open Current.Syntax in
  let spec_str =
    let+ build_spec = build_spec in
    Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true (build_spec |> Spec.finish)
  in
  `Contents spec_str

let build ?label ?cache_hint t ~pool ~src spec =
  match profile with
  | `Production | `Dev ->
      to_obuilder_job spec |> Current_ocluster.build_obuilder ?label ?cache_hint t ~pool ~src
  | `Docker ->
      let options =
        {
          Cluster_api.Docker.Spec.build_args = [];
          squash = false;
          buildkit = true;
          include_git = true;
        }
      in
      to_docker_job spec |> Current_ocluster.build ~options ?label ?cache_hint t ~pool ~src
