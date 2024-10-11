let pool = Current.Pool.create ~label:"git checkout" 8

module GitPush = struct
  type t = No_context

  module Key = struct
    type t = { store : Git_store.t; branch : string }

    let digest t = Git_store.remote t.store ^ "#" ^ t.branch
  end

  module Value = struct
    type t = Current_git.Commit.t list

    let digest t =
      let json =
        `List (List.map (fun x -> `String (Current_git.Commit.hash x)) t)
      in
      Yojson.to_string json
  end

  module Outcome = struct
    type t = Current_git.Commit_id.t

    type info = { repo : string; hash : string; gref : string }
    [@@deriving yojson]

    let t_of_info { repo; gref; hash } =
      Current_git.Commit_id.v ~repo ~gref ~hash

    let info_of_t t =
      let open Current_git.Commit_id in
      { repo = repo t; hash = hash t; gref = gref t }

    let marshal t = t |> info_of_t |> info_to_yojson |> Yojson.Safe.to_string

    let unmarshal t =
      t
      |> Yojson.Safe.from_string
      |> info_of_yojson
      |> Result.get_ok
      |> t_of_info
  end

  let auto_cancel = true
  let pp f _ = Fmt.string f "Monorepo git push"
  let id = "mirage-ci-monorepo-git-push"

  let publish No_context job { Key.store; branch } commits =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let* () = Current.Job.start ~level:Average job in
    let** () = Git_store.sync ~job store in
    Git_store.with_clone ~job ~branch store @@ fun tmpdir ->
    let cmd cmd =
      Current.Process.exec ~cwd:tmpdir ~cancellable:true ~job ("", cmd)
    in
    let read cmd =
      Current.Process.check_output ~cwd:tmpdir ~cancellable:true ~job ("", cmd)
    in
    let** () = cmd [| "git"; "rm"; "*"; "--ignore-unmatch" |] in
    let** () = cmd [| "touch"; ".gitmodules" |] in
    let** _ =
      Lwt_list.fold_left_s
        (fun status commit ->
          match status with
          | Ok () ->
              Current_git.with_checkout ~pool ~job commit @@ fun commit_dir ->
              let repo =
                commit |> Current_git.Commit.id |> Current_git.Commit_id.repo
              in
              let branch =
                commit |> Current_git.Commit.id |> Current_git.Commit_id.gref
              in
              let repo_name =
                repo |> Filename.basename |> Filename.remove_extension
              in
              let** () =
                cmd
                  [|
                    "cp";
                    "-R";
                    Fpath.to_string commit_dir;
                    Fpath.(to_string (tmpdir / repo_name));
                  |]
              in
              let** () =
                cmd
                  [|
                    "git";
                    "submodule";
                    "add";
                    "-f";
                    "-b";
                    branch;
                    repo;
                    repo_name;
                  |]
              in
              Lwt.return_ok ()
          | err -> Lwt.return err)
        (Ok ()) commits
    in
    let* result =
      cmd
        [|
          "git";
          "commit";
          "-m";
          "monorepo-git-push";
          "--author";
          "Mirage CI pipeline <ci@mirageos.org>";
        |]
    in
    let** () =
      match result with
      | Ok () -> cmd [| "git"; "push"; "--force"; "origin"; branch |]
      | Error _ ->
          Current.Job.log job "Nothing was commited.";
          Lwt.return_ok ()
    in
    let** hash = read [| "git"; "rev-parse"; "HEAD" |] in
    Lwt.return_ok
      (Current_git.Commit_id.v
         ~repo:(Git_store.http_remote store)
         ~gref:branch ~hash)
end

module GitPushCache = Current_cache.Output (GitPush)

let v git_store ~branch commits =
  let open Current.Syntax in
  Current.component "Monorepo git push"
  |> let> commits = commits in
     GitPushCache.set No_context { store = git_store; branch } commits
