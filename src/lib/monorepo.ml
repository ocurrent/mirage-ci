open Current.Syntax

(********************************************)
(*****************  LOCK  *******************)
(********************************************)

type t = { solver : Current_solver.t }

let v ~system ~repos =
  let+ solver = Current_solver.v ~system ~repos ~packages:[ "opam-monorepo" ] in
  { solver }

let lock_spec ~system ~repos ~opam =
  let open Obuilder_spec in
  let opam_monorepo =
    Platform.spec system
    |> Spec.add (Setup.add_repositories repos)
    |> Spec.add (Setup.install_tools [ "opam-monorepo" ])
    |> Spec.add [ run "sudo cp $(opam var bin)/opam-monorepo /opam-monorepo" ]
    |> Spec.finish
  in
  Platform.spec system
  |> Spec.add (Setup.add_repositories repos)
  |> Spec.children ~name:"monorepo" opam_monorepo
  |> Spec.add
       [
         workdir "/src";
         run "sudo chown opam:opam /src";
         run "echo '%s' >> monorepo.opam" (Opamfile.marshal opam);
         copy ~from:(`Build "monorepo") [ "/opam-monorepo" ] ~dst:"/usr/local/bin/opam-monorepo";
         run "opam-monorepo lock -l monorepo.opam.locked";
         run "cp monorepo.opam.locked monorepo.opam";
         run "opam pin add monorepo -ny -k path /src/ --ignore-pin-depends";
         run
           "opam list --columns name:,dev-repo: --required-by monorepo -s --separator ';' >> \
            monorepo.dev-repo";
       ]

let upload_spec ~store ~branch =
  let open Obuilder_spec in
  let open Git_store in
  Spec.add
    [
      Cluster.clone ~branch ~directory:"store" store;
      run "cp monorepo.opam.locked monorepo.dev-repo  store/";
      workdir "store";
      run "git add monorepo.opam.locked monorepo.dev-repo";
      run
        "git diff-index --quiet HEAD || git commit -m 'Update monorepo.opam.locked and \
         monorepo.dev-repo'";
      Cluster.push store;
    ]

module Reader : Git_store.Reader with type t = string * string = struct
  type t = string * string [@@deriving yojson]

  let id = "monorepo.opam.locked-dev-repo"

  let fn dir =
    let lockfile = Bos.OS.File.read Fpath.(dir / "monorepo.opam.locked") |> Result.get_ok in
    let dev_repo = Bos.OS.File.read Fpath.(dir / "monorepo.dev-repo") |> Result.get_ok in
    Lwt.return (lockfile, dev_repo)

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal t = Yojson.Safe.from_string t |> of_yojson |> Result.get_ok

  let pp f (lockfile, dev_repo) = Fmt.pf f "Lockfile:\n%s\n\nDev-repo:\n%s\n" lockfile dev_repo
end

let lock ~key ~cluster ~store ~repos ~opam  ~system =
  let spec =
    let+ opam = opam and+ repos = repos in
    lock_spec ~system ~repos ~opam |> upload_spec ~store ~branch:key
  in
  let job = Config.build cluster ~pool:"linux-x86_64" ~src:(Current.return []) spec in
  let k =
    let+ spec = spec and+ _ = job (* fake dependency on job *) in
    Fmt.to_to_string Obuilder_spec.pp (spec |> Spec.finish)
  in
  Git_store.read ~branch:key (module Reader) store k

let lock ~key ~value ~cluster ~store ~repos ~opam ~system _ =
  let lock =
    let+ lockfile, dev_repos_str = lock ~key ~cluster ~store ~repos ~opam  ~system in
    let lockfile = Opamfile.unmarshal lockfile in
    Monorepo_lock.make ~opam_file:lockfile
      ~dev_repo_output:(String.split_on_char '\n' dev_repos_str)
  in
  Current.collapse ~key:"monorepo-lock" ~value ~input:opam lock

(********************************************)
(***************   SPEC       ***************)
(********************************************)

let spec ~base ~lock () =
  let+ lock = lock and+ base = base in
  let opamfile = Monorepo_lock.lockfile lock in
  let open Obuilder_spec in
  base
  |> Spec.add (Setup.install_tools [ "dune"; "opam-monorepo" ])
  |> Spec.add
       [
         workdir "/src";
         run "sudo chown opam:opam /src";
         run "echo '%s' >> monorepo.opam" (Opamfile.marshal opamfile);
         (* depexts  *)
         run "opam pin -n add monorepo . --locked --ignore-pin-depends";
         run ~network:Setup.network "opam install -y monorepo";
         run "opam pin -n remove monorepo";
         (* setup lockfile *)
         run "cp monorepo.opam monorepo.opam.locked";
         run ~network:Setup.network "opam exec -- opam monorepo pull -y -l  monorepo.opam.locked";
       ]

(********************************************)
(********************************************)
(********************************************)

let opam_file ~ocaml_version (projects : Universe.Project.t list) =
  let pp_project f (proj : Universe.Project.t) =
    List.iter (fun opam -> Fmt.pf f "\"%s\"\n" opam.Universe.name) proj.opam
  in
  Fmt.str
    {|
opam-version: "2.0"
depends: [
  "ocaml" { = "%s"}
  %a
]
conflicts: [
  "parsexp" { < "v0.14.0"}
  "sexplib" { < "v0.14.0"}
  "base" { < "v0.14.0"}
]
|}
    ocaml_version (Fmt.list pp_project) projects
  |> Opamfile.unmarshal
