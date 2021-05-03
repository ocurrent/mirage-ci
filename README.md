# Mirage CI

This CI is a set of ocurrent pipelines testing various things for the Mirage project.

In `src/pipelines/`, there are three kind of pipelines:

- `monorepo`: assemble monorepos and use `dune` to test the buildability of mirage projects as 
  released packages but also by testing the development branches altogether.
- `skeleton`: test the mirage project using the `mirage-skeleton` unikernel repository, by performing
  a multi-stage set of builds.
- `PR`: test PRs against `mirage/mirage`, `mirage/mirage-dev` and `mirage/mirage-skeleton`. 

## Running

You need an ocluster submission token and a git server on which monorepo can be pushed. 
For the main CI, obtain a Github personal access token that has the `repo:status` authorisation and save it in a file. 

```
dune exec -- mirage-ci \
    --ocluster-cap <OCLUSTER_SUBMISSION_TOKEN_FILE> \
    --github-token-file <GITHUB_TOKEN_FILE> \
    --monorepo-pull-from https://github.com/XXX/mirage-ci-monorepo.git \
    --monorepo-push-to git@github.com:XXX/mirage-ci-monorepo.git \
    --test-mirage-4 \
    --main-ci mirage-master,skeleton-dev,mirage-dev-master
```

Then, use `dune exec -- mirage-ci --github-token-file <TOKEN_FILE>` to launch the CI pipeline. 
