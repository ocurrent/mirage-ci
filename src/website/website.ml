module Git = Current_git
module Github = Current_github

module Website_description = struct
  open Tyxml_html

  let extra_routes = []

  module Output = struct
    type t = unit

    let marshal () = ""
    let unmarshal _ = ()

    open Tyxml_html

    let render_inline () = txt ""
  end

  module Node = struct
    type t = string

    let render_inline name = txt name
    let map_status _ = Fun.id
    let marshal = Fun.id
    let unmarshal = Fun.id
  end

  module Stage = struct
    type t = string

    let id name = name
    let render_inline name = txt name
    let render _ = txt ""
    let marshal = Fun.id
    let unmarshal = Fun.id
  end

  module Pipeline = struct
    module Group = struct
      type t =
        | Local
        | Mirage
        | Mirage_dev
        | Mirage_skeleton
        | Opam_overlays
        | Mirage_opam_overlays

      let id = function
        | Local -> "local"
        | Mirage -> "mirage/mirage"
        | Mirage_dev -> "mirage/mirage-dev"
        | Mirage_skeleton -> "mirage/mirage-skeleton"
        | Opam_overlays -> "dune-universe/opam-overlays"
        | Mirage_opam_overlays -> "dune-universe/mirage-opam-overlays"

      let to_string = id
    end

    module Source = struct
      let build_mode_to_string = function
        | `Mirage_3 -> "mirage-3"
        | `Mirage_4 -> "mirage-4"

      let branch_name ref =
        match String.split_on_char '/' ref with
        | [ "refs"; "heads"; b ] -> b
        | _ -> "failure"

      (* the exposed metadata *)
      type metadata_gh = {
        kind :
          [ `Mirage | `Mirage_dev | `Mirage_skeleton | `Overlay of string ];
        build_mode : [ `Mirage_4 | `Mirage_3 ];
        commit : string;
        ref : Github.Api.Ref.t;
        owner : string;
        name : string;
        friend_prs : Github.Api.Ref.pr_info list;
      }

      let gh_id = function
        | { ref = `PR { id; _ }; owner; name; commit; build_mode; _ } ->
            Fmt.str "pr-%d-%s-%s-%s-%s" id
              (build_mode_to_string build_mode)
              owner name commit
        | { ref = `Ref ref; owner; name; commit; build_mode; _ } ->
            Fmt.str "branch-%s-%s-%s-%s-%s" (branch_name ref)
              (build_mode_to_string build_mode)
              owner name commit

      type t = [ `Local of [ `Mirage_4 | `Mirage_3 ] | `Github of metadata_gh ]

      let group = function
        | `Local _ -> Group.Local
        | `Github { kind = `Mirage; _ } -> Mirage
        | `Github { kind = `Mirage_dev; _ } -> Mirage_dev
        | `Github { kind = `Mirage_skeleton; _ } -> Mirage_skeleton
        | `Github { kind = `Overlay "opam-overlays"; _ } -> Opam_overlays
        | `Github { kind = `Overlay "mirage-opam-overlays"; _ } -> Mirage_opam_overlays
        | `Github { kind = `Overlay _; _ } -> failwith "unknown overlay repository"

      let build_mode_to_string = function
        | `Mirage_3 -> "mirage-3"
        | `Mirage_4 -> "mirage-4"

      let branch_name ref =
        match String.split_on_char '/' ref with
        | [ "refs"; "heads"; b ] -> b
        | _ -> "failure"

      let id = function
        | `Local `Mirage_3 -> "local-mirage-3"
        | `Local `Mirage_4 -> "local-mirage-4"
        | `Github { ref = `PR { id; _ }; owner; name; build_mode; _ } ->
            Fmt.str "pr-%d-%s-%s-%s" id
              (build_mode_to_string build_mode)
              owner name
        | `Github { ref = `Ref ref; owner; name; build_mode; _ } ->
            Fmt.str "branch-%s-%s-%s-%s" (branch_name ref)
              (build_mode_to_string build_mode)
              owner name

      let to_string = function
        | `Local `Mirage_3 -> "Local (mirage 3)"
        | `Local `Mirage_4 -> "Local (mirage 4)"
        | `Github { ref = `PR { id; _ }; _ } -> Fmt.str "PR #%d" id
        | `Github { ref = `Ref ref; _ } -> Fmt.str "Branch %s" (branch_name ref)
    end

    type t = Source.t

    let to_link (t : t) =
      match t with
      | `Local _ -> ""
      | `Github { ref = `PR { id; _ }; owner; name; _ } ->
          Fmt.str "https://github.com/%s/%s/pull/%d" owner name id
      | `Github { ref = `Ref b; owner; name; _ } ->
          Fmt.str "https://github.com/%s/%s/tree/%s" owner name b

    let build_mode_to_string = function
      | `Mirage_3 -> "mirage-3"
      | `Mirage_4 -> "mirage-4"

    let id = function
      | `Local `Mirage_3 -> "local-mirage-3"
      | `Local `Mirage_4 -> "local-mirage-4"
      | `Github gh -> Source.gh_id gh

    let marshal v = Marshal.to_string v []
    let unmarshal v = Marshal.from_string v 0
    let source = Fun.id

    let branch_name ref =
      match String.split_on_char '/' ref with
      | [ "refs"; "heads"; b ] -> b
      | _ -> "failure"

    let render_inline (t : t) =
      match t with
      | `Local b -> txt (Fmt.str "%s" (build_mode_to_string b))
      | `Github { commit; _ } ->
          let commit_hash = String.sub commit 0 7 in
          txt ("@" ^ commit_hash)

    let render (t : t) =
      let pr_name =
        match t with
        | `Github { ref = `PR { title; _ }; _ } -> h2 [ txt title ]
        | _ -> txt ""
      in
      div
        [
          pr_name; txt "Link to "; a ~a:[ a_href (to_link t) ] [ txt "Github" ];
        ]
  end

  let render_index () =
    div
      [
        h1 [ txt "Mirage CI" ];
        span
          [
            txt "Source code available here: ";
            a
              ~a:[ a_href "https://github.com/ocurrent/mirage-ci" ]
              [ txt "https://github.com/ocurrent/mirage-ci" ];
          ];
        br ();
        span [ txt "@TheLortex: I'm rolling out a new UI this week (29 nov)" ];
      ]
end

include Current_web_pipelines.Web.Make (Website_description)
