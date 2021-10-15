type t = {
  ssh_host : string;
  ssh_port : int option;
  ssh_repo : string;
  http_remote : string;
  private_key : string;
  public_key : string;
  pool : unit Current.Pool.t;
}

open Cmdliner

let git_ssh_host =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The git SSH host to store the transient data" ~docv:"HOST"
       [ "git-ssh-host" ]

let git_ssh_port =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info ~doc:"The git SSH port" ~docv:"PORT" [ "git-ssh-port" ]

let git_ssh_repository =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info
       ~doc:
         "The git repository to store the transient data on the specified host"
       ~docv:"REPO" [ "git-ssh-repo" ]

let git_http_remote =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The public http remote for the storage repository"
       ~docv:"HOST" [ "git-http-remote" ]

let private_key_file =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"A private key to use to access the remote" ~docv:"FILE"
       [ "privkey" ]

let public_key_file =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"A public key to use to access the remote" ~docv:"FILE"
       [ "pubkey" ]

let load_file path =
  try
    let ch = open_in path in
    let len = in_channel_length ch in
    let data = really_input_string ch len in
    close_in ch;
    data
  with ex ->
    if Sys.file_exists path then
      failwith @@ Fmt.str "Error loading %S: %a" path Fmt.exn ex
    else failwith @@ Fmt.str "File %S does not exist" path

let v ssh_host ssh_port ssh_repo http_remote private_key_file public_key_file =
  {
    ssh_host;
    ssh_port;
    ssh_repo;
    http_remote;
    private_key = load_file private_key_file;
    public_key = load_file public_key_file;
    pool = Current.Pool.create ~label:"git repo" 1;
  }

let cmdliner =
  Term.(
    const v
    $ git_ssh_host
    $ git_ssh_port
    $ git_ssh_repository
    $ git_http_remote
    $ private_key_file
    $ public_key_file)

let v ~ssh_host ?ssh_port ~ssh_repo ~http_remote ~private_key_file
    ~public_key_file =
  v ssh_host ssh_port ssh_repo http_remote private_key_file public_key_file

let remote t = Fmt.str "git@%s:%s" t.ssh_host t.ssh_repo
let http_remote t = t.http_remote

let git_checkout_or_create b =
  Fmt.str
    "(git remote set-branches --add origin %s && git fetch origin %s && git \
     checkout --track origin/%s) || (git checkout -b %s && git push \
     --set-upstream origin %s)"
    b b b b b

module Cluster = struct
  let secrets =
    Obuilder_spec.Secret.
      [
        v ~target:"/home/opam/.ssh/id_rsa" "ssh_privkey";
        v ~target:"/home/opam/.ssh/id_rsa.pub" "ssh_pubkey";
        v ~target:"/home/opam/.ssh/config" "ssh_config";
      ]

  let config t =
    Fmt.str
      {|Host %s
              IdentityFile ~/.ssh/id_rsa
              %a
              User git
              StrictHostKeyChecking=no
            |}
      t.ssh_host
      Fmt.(option (fmt "Port %d"))
      t.ssh_port

  let clone ~branch ~directory t =
    Obuilder_spec.run ~network:[ "host" ] ~secrets
      "git clone --single-branch %s %s && cd %s && (%s)" (remote t) directory
      directory
      (git_checkout_or_create branch)

  let push ?(force = false) _ =
    Obuilder_spec.run ~network:[ "host" ] ~secrets
      (if force then "git push -f" else "git push")

  let secrets t =
    [
      ("ssh_privkey", t.private_key);
      ("ssh_pubkey", t.public_key);
      ("ssh_config", config t);
    ]
end

module type Reader = sig
  type t

  val pp : t Fmt.t
  val id : string
  val fn : Fpath.t -> t Lwt.t
  val marshal : t -> string
  val unmarshal : string -> t
end

let id_of_repo repo =
  let pp_hex f d =
    for x = 0 to Cstruct.len d - 1 do
      let byte = Cstruct.get_uint8 d x in
      Fmt.pf f "%02x" byte
    done
  in
  let module Hash = Mirage_crypto.Hash.SHA256 in
  let base = Filename.basename repo in
  let digest = Hash.digest (Cstruct.of_string repo) in
  Fmt.str "%s-%a" base pp_hex digest

let repo_folder repo =
  let state_dir = Current.state_dir "git-store" in
  Fpath.(state_dir / id_of_repo repo)

let sync ~job t =
  let open Lwt.Syntax in
  let repo = remote t in
  let state_folder = repo_folder repo in
  let switch = Current.Switch.create ~label:"git sync" () in
  let* () = Current.Job.use_pool ~switch job t.pool in
  let* result =
    match Bos.OS.Path.exists state_folder with
    | Error _ ->
        Fmt.failwith "Failed to look for git folder %a" Fpath.pp state_folder
    | Ok false ->
        Current.Process.exec ~cancellable:false ~job
          ( "",
            [| "git"; "clone"; "--bare"; repo; Fpath.to_string state_folder |]
          )
    | Ok true ->
        Current.Process.exec ~cwd:state_folder ~cancellable:false ~job
          ("", [| "git"; "fetch"; "-f"; "origin"; "*:*" |])
  in
  let* () = Current.Switch.turn_off switch in
  Lwt.return result

let with_clone ~job ~branch store fn =
  let ( let** ) = Lwt_result.bind in
  let repo = remote store in
  let state_folder = repo_folder repo in
  Current.Process.with_tmpdir @@ fun tmpdir ->
  let** () =
    Current.Process.exec ~cancellable:false ~job
      ( "",
        [|
          "git";
          "clone";
          "--single-branch";
          Fpath.to_string state_folder;
          Fpath.to_string tmpdir;
        |] )
  in
  let** () =
    Current.Process.exec ~cwd:tmpdir ~cancellable:false ~job
      ("", [| "sh"; "-c"; git_checkout_or_create branch |])
  in
  let** () =
    Current.Process.exec ~cwd:tmpdir ~cancellable:false ~job
      ("", [| "git"; "remote"; "set-url"; "origin"; repo |])
  in
  fn tmpdir

module ReadOp (R : Reader) = struct
  type store = t
  type t = No_context

  let pp f _ = Fmt.pf f "git store"
  let id = "git-store-" ^ R.id

  module Key = struct
    type t = { key : string; branch : string; store : store }

    let digest t =
      let repo = remote t.store in
      t.key ^ "-" ^ t.branch ^ repo
  end

  module Value = R

  let auto_cancel = true

  let build No_context job { Key.branch; store; _ } =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let* () = Current.Job.start ~level:Mostly_harmless job in
    let** () = sync ~job store in
    with_clone ~job ~branch store @@ fun tmpdir ->
    let* result = R.fn tmpdir in
    Current.Job.log job "%a" R.pp result;
    Lwt.return_ok result
end

let read (type a) ~branch (module R : Reader with type t = a) store key :
    a Current.t =
  let module Read = ReadOp (R) in
  let module Cache = Current_cache.Make (Read) in
  let open Current.Syntax in
  Current.component "read git store"
  |> let> key = key in
     Cache.get No_context { Read.Key.key; branch; store }
