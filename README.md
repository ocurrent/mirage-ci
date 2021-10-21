# Mirage CI

This CI is a set of [ocurrent](https://github.com/ocurrent/ocurrent)
pipelines testing various things for the Mirage project.

In `src/pipelines/`, there are two kind of pipelines:

- **monorepo**: assemble monorepos and use `dune` to test the
  buildability of mirage projects as released packages but also by
  testing the development branches altogether.
- **PR**: test branches and PRs against:
  - [mirage/mirage](https://github.com/mirage/mirage)
  - [mirage/mirage-dev](https://github.com/mirage/mirage-dev)
  - [mirage/mirage-skeleton](https://github.com/mirage/mirage-skeleton)
  - [mirage/opam-overlays](https://github.com/mirage/opam-overlays)

By default, two testing workflows are implemented:
- *Mirage3*, correspinding to the `--test-mirage-3` CLI option. This is testing:
  - [mirage#3](https://github.com/mirage/mirage/tree/3)
  - [mirage-dev#3](https://github.com/mirage/mirage-dev/tree/3)
  - [mirage-skeleton#master](https://github.com/mirage/mirage-skeleton/tree/master)
  - no opam overlays
- *Mirage4*, correspinding to the `--test-mirage-3` CLI option. This is testing:
  - [mirage#main](https://github.com/mirage/mirage)
  - [mirage-dev#master](https://github.com/mirage/mirage-dev)
  - [mirage-skeleton#mirage-dev](https://github.com/mirage/mirage-skeleton/tree/mirage-dev)
  - [opam-overlays#master](https://github.com/mirage/opam-overlays)

**co-dependent PRs** `mirage-ci` will detect if PRs submitted to these
repositories mention each other. In that case, they will be
tested together.

## Running

You need an ocluster submission token and a git server on which
monorepo can be pushed. For the central CI, obtain a Github personal
access token with the `repo:status` authorisation and save it in a
file.

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

## Deploying

The `live` branch will automatically be deployed to
[ci.mirage.io](https://ci.mirage.io/).

To (re)-configure the live-deployer, log on `ci.mirage.io` and run:

```
$ git clone -b live https://github.com/ocurrent/mirage-ci.git
$ cd mirage-ci
$ docker build . -t mirage-ci
$ docker service create \
  --name infra_mirage-ci \
  -p 8082:8080 \
  -e CI_PROFILE=production \
  --mount type=bind,ro,source=/home/camel/.ssh,destination=/ssh \
  --mount type=bind,ro,source=/home/camel/mirage-ci/cap,destination=/cap \
  --mount type=bind,source=/var/run/docker.sock,destination=/var/run/docker.sock \
  --mount type=volume,source=mirage-ci,destination=/var/lib/ocurrent \
  mirage-ci \
  --ocluster-cap /cap/mirage-ci.cap \
  --github-token-file /cap/github_mirage \
  --git-ssh-host ci.mirage.io \
  --git-ssh-repo mirage-ci/mirage-monorepo.git \
  --git-ssh-port 10022 \
  --git-http-remote=https://ci.mirage.io/git/mirage-ci/mirage-ci-monorepo.git \
  --privkey /ssh/git \
  --pubkey /ssh/git.pub \
  --test-mirage-4 mirage,skeleton,dev,overlay \
  --test-mirage-3 mirage,skeleton,dev,overlay \
  --test-monorepos \
  --self-deploy
```

## Local testing

It is possible to test the pipeline locally, but it will still clone
the repositories from GitHub. To do so:

```
dune exec -- mirage-ci-local --test-mirage-3 --test-mirage-4
```

## PR testing

To enable status reporting for commits, make sure you have the proper
authorisation on mirage repositories, then add the selected
repositories where commit status should be reported. For instance,
`--test-mirage-4 mirage,skeleton,dev,overlay` will update the status
of all the watched repositories.
