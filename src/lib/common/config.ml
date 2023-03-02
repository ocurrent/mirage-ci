let profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "dev" | None -> `Dev
  | Some "docker" -> `Docker
  | Some x -> Fmt.failwith "Unknown $PROFILE setting %S" x

type build_config =
  | Config_dry
  | Config_local
  | Config_cluster of {
      connection : Current_ocluster.Connection.t;
      profile : [ `Production | `Docker ];
    }

let cmdliner =
  let open Cmdliner in
  let apply f = Cmdliner.Term.(app (const f)) in
  let ocluster_cap ~profile =
    Arg.required
    @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
    @@ Arg.info ~doc:"The ocluster submission capability file" ~docv:"FILE"
         [ "ocluster-cap" ]
    |> apply (fun cap ->
           let vat = Capnp_rpc_unix.client_only_vat () in
           let submission_cap = Capnp_rpc_unix.Vat.import_exn vat cap in
           Config_cluster
             {
               connection =
                 Current_ocluster.Connection.create ~max_pipeline:20
                   submission_cap;
               profile;
             })
  in
  let dry_mode =
    Arg.value @@ Arg.flag @@ Arg.info ~doc:"Do not run the tests" [ "dry" ]
    |> apply (function true -> Config_dry | false -> Config_local)
  in
  match profile with
  | `Dev -> dry_mode
  | (`Production | `Docker) as profile -> ocluster_cap ~profile

type t =
  | Dry
  | Local of { secrets : (string * string) list }
  | Cluster of {
      ocluster : Current_ocluster.t;
      profile : [ `Production | `Docker ];
    }

let timeout = Duration.of_hour 1

let make ?(secrets = []) build_config =
  match build_config with
  | Config_dry -> Dry
  | Config_local -> Local { secrets }
  | Config_cluster { connection; profile } ->
      Cluster
        {
          ocluster =
            Current_ocluster.v ~timeout ~secrets ~urgent:`Never connection;
          profile;
        }

let to_obuilder_job build_spec =
  let open Current.Syntax in
  let+ build_spec = build_spec in
  let spec_str =
    Fmt.to_to_string Obuilder_spec.pp (build_spec |> Spec.finish)
  in
  let open Cluster_api.Obuilder_job.Spec in
  { spec = `Contents spec_str }

let to_docker_job build_spec =
  let open Current.Syntax in
  let spec_str =
    let+ build_spec = build_spec in
    Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true ~os:`Unix
      (build_spec |> Spec.finish)
  in
  `Contents spec_str

let pool = Current.Pool.create ~label:"docker build" 1

let local_build ?label ~secrets ~src spec =
  let open Current.Syntax in
  let dockerfile =
    let _ = Bos.OS.Dir.create (Fpath.v "/tmp/mirage-ci") in
    let dockerfile =
      let+ spec = spec in
      Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true (Spec.finish spec)
        ~os:`Unix
    in
    let path =
      let+ dockerfile = dockerfile in
      let hash = Digest.string dockerfile |> Digest.to_hex in
      Fpath.v (Fmt.str "/tmp/mirage-ci/Dockerfile.%s" hash)
    in
    let+ () = Current_fs.save path dockerfile and+ path = path in
    `File path
  in
  let secrets_args =
    List.map
      (fun (id, src) -> [ "--secret"; "id=" ^ id ^ ",src=" ^ src ])
      secrets
    |> List.concat
  in
  let src =
    match src with
    | [] -> `No_context
    | [ v ] -> `Git (Current_git.fetch v)
    | _ -> `Git (Current.fail "multiple sources is unsupported")
  in
  Current_docker.Default.build ?label ~pool ~pull:false ~build_args:secrets_args
    ~dockerfile src
  |> Current.ignore_value

let dry_return ?label ~pool input =
  let open Current.Syntax in
  Current.component "dry: %a (%s)" Fmt.(option string) label pool
  |> let> () = input in
     Current.Primitive.const ()

let build ?label ?cache_hint context ~pool ~src spec =
  let pool_is_available = function
    | "linux-x86_64" when Platform.platform_host.arch = Amd64 -> true
    | "linux-arm64" when Platform.platform_host.arch = Arm64 -> true
    | _ -> false
  in
  match context with
  | Local { secrets } when pool_is_available pool ->
      local_build ?label ~secrets ~src spec
  | Local _ -> Current.fail (Fmt.str "Platform %s is not host's platform" pool)
  | Dry -> dry_return ?label ~pool (Current.ignore_value spec)
  | Cluster { profile = `Production; ocluster } ->
      to_obuilder_job spec
      |> Current_ocluster.build_obuilder ?label ?cache_hint ocluster ~pool
           ~src:(Current.list_seq src)
      |> Current.map ignore
  | Cluster { profile = `Docker; ocluster } ->
      let options =
        {
          Cluster_api.Docker.Spec.build_args = [];
          squash = false;
          buildkit = true;
          include_git = true;
        }
      in
      to_docker_job spec
      |> Current_ocluster.build ~options ?label ?cache_hint ocluster ~pool
           ~src:(Current.list_seq src)
      |> Current.map ignore
