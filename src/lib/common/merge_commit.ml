module Git = Current_git

type t = Git.Commit_id.t * Git.Commit_id.t list

let make ~base merge_commits = (base, merge_commits)
let no_merge base = (base, [])

let spec (base, merges) =
  let open Obuilder_spec in
  Spec.minimal "alpine"
  |> Spec.add
       ([
          shell [ "/bin/sh"; "-c" ];
          run ~network:[ "host" ] "apk add git";
          workdir "/merged";
          run ~network:[ "host" ]
            "git config --global user.email \"none@none.none\" && git config \
             --global user.name \"None\"";
          run ~network:[ "host" ]
            "git clone --recursive %S /merged && git fetch origin %S && git \
             reset --hard %s"
            (Git.Commit_id.repo base) (Git.Commit_id.gref base)
            (Git.Commit_id.hash base);
        ]
       @ List.map
           (fun merge ->
             run ~network:[ "host" ] "git fetch origin %S && git merge %s"
               (Git.Commit_id.gref merge) (Git.Commit_id.hash merge))
           merges)
  |> Spec.finish

let to_list (base, merge) = base :: merge
