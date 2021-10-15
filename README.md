# Mirage CI

This CI is a set of ocurrent pipelines testing various things for the Mirage project.

In `src/pipelines/`, there are two kind of pipelines:

- `monorepo`: assemble monorepos and use `dune` to test the buildability of mirage projects as 
  released packages but also by testing the development branches altogether.
- `PR`: test PRs against `mirage/mirage`, `mirage/mirage-dev` and `mirage/mirage-skeleton`. 

## Running

You need an ocluster submission token and a git server on which monorepo can be pushed. 
For the main CI, obtain a Github personal access token that has the `repo:status` authorisation and save it in a file. 

Then use:
```
dune exec -- mirage-ci \
    --ocluster-cap <OCLUSTER_SUBMISSION_TOKEN_FILE> \
    --github-token-file <GITHUB_TOKEN_FILE> \
    --git-http-remote https://github.com/XXX/mirage-ci-monorepo.git \
    --git-ssh-host github.com \
    --git-ssh-repo XXX/mirage-ci-monorepo.git \
    --privkey ~/.ssh/key \
    --pubkey ~/.ssh/key.pub \
    --test-monorepos \
    --test-mirage-4
```

## Local testing

Without any token, it's possible to test the main pipeline.
```
dune exec -- mirage-ci-local --test-mirage-3 --test-mirage-4
```

## PR testing

To enable commit status reporting, make sure you have the proper authorization on mirage repositories, 
then add the selected repositories where commit status should be reported. Example: `--test-mirage-4 mirage,skeleton,dev` 
to report status for all the watched repositories.

When `--test-mirage-4` is used, `mirage#main` is tested with `mirage-dev#master` on `mirage-skeleton#mirage-dev`.

When `--test-mirage-3` is used, `mirage#3` is tested with `mirage-dev#3` on `mirage-skeleton#master`.

If two PRs are co-dependent, they should be tested together. In that situation the author can mention, for each PR, the other one
in the description: when the CI sees a link to an other PR, it uses it for testing instead of the main branch.  
