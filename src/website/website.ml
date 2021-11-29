module Git = Current_git
module Github = Current_github

module Website_description = struct
  open Tyxml_html

  let extra_routes = []

  module Output = struct
    type t = unit

    open Tyxml_html

    let render_inline () = txt ""
  end

  module Node = struct
    type t = string

    let render_inline name = txt name
    let map_status _ = Fun.id
  end

  module Stage = struct
    type t = string

    let id name = name
    let render_inline name = txt name
    let render _ = txt ""
  end

  module Pipeline = struct
    type t = Mirage_ci_pipelines.PR.pipeline

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

    let id (t : t) = Mirage_ci_pipelines.PR.id t

    let branch_name ref =
      match String.split_on_char '/' ref with
      | [ "refs"; "heads"; b ] -> b
      | _ -> "failure"

    let render_inline (t : t) =
      match t with
      | `Local b -> txt (Fmt.str "Local build (%s)" (build_mode_to_string b))
      | `Github { ref = `PR { id; _ }; owner; name; commit; _ } ->
          let commit_hash = String.sub commit 0 7 in
          txt (Fmt.str "PR %d on %s/%s @@ %s" id owner name commit_hash)
      | `Github { ref = `Ref ref; owner; name; commit; _ } ->
          let commit_hash = String.sub commit 0 7 in
          txt
            (Fmt.str "Branch %s of %s/%s @@ %s" (branch_name ref) owner name
               commit_hash)

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
