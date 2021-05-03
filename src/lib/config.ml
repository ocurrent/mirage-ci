let to_ocluster_spec build_spec =
  let open Current.Syntax in
  let+ build_spec = build_spec in
  let spec_str = Fmt.to_to_string Obuilder_spec.pp (build_spec |> Spec.finish) in
  let open Cluster_api.Obuilder_job.Spec in
  { spec = `Contents spec_str }
