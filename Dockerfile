FROM ocaml/opam:debian-11-ocaml-4.14@sha256:5ce947a2707d3cfe0d2a8918ef41c8b5f88ccddfcace11871c5f553afac189ed AS build
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam && opam init --reinit -ni
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto graphviz m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git pull origin master && git reset --hard 5095d4fd3728a0b7783c24f4b45dbe663c9f4d7b && opam update
COPY --chown=opam \
	vendor/ocurrent/current_docker.opam \
	vendor/ocurrent/current_github.opam \
	vendor/ocurrent/current_git.opam \
	vendor/ocurrent/current.opam \
	vendor/ocurrent/current_rpc.opam \
	vendor/ocurrent/current_slack.opam \
	vendor/ocurrent/current_web.opam \
	/src/vendor/ocurrent/
WORKDIR /src
RUN opam pin add -yn current_docker.dev "./vendor/ocurrent" && \
    opam pin add -yn current_github.dev "./vendor/ocurrent" && \
    opam pin add -yn current_git.dev "./vendor/ocurrent" && \
    opam pin add -yn current.dev "./vendor/ocurrent" && \
    opam pin add -yn current_rpc.dev "./vendor/ocurrent" && \
    opam pin add -yn current_slack.dev "./vendor/ocurrent" && \
    opam pin add -yn current_web.dev "./vendor/ocurrent"
COPY --chown=opam mirage-ci.opam /src/
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN --mount=type=cache,target=./_build/,uid=1000,gid=1000 opam config exec -- dune build ./_build/install/default/bin/mirage-ci ./_build/install/default/bin/mirage-ci-solver && cp ./_build/install/default/bin/mirage-ci ./_build/install/default/bin/mirage-ci-solver .

FROM debian:11
RUN apt-get update && apt-get install libev4 openssh-client curl gnupg2 dumb-init git graphviz libsqlite3-dev ca-certificates netbase gzip bzip2 xz-utils unzip tar -y --no-install-recommends
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add -
RUN echo 'deb [arch=amd64] https://download.docker.com/linux/debian buster stable' >> /etc/apt/sources.list
RUN apt-get update && apt-get install docker-ce docker-buildx-plugin -y --no-install-recommends
RUN git config --global user.name "mirage" && git config --global user.email "ci"
WORKDIR /var/lib/ocurrent
ENTRYPOINT ["dumb-init", "/usr/local/bin/mirage-ci"]
ENV OCAMLRUNPARAM=a=2
ENV DOCKER_BUILDKIT=1
COPY --from=build /src/mirage-ci /src/mirage-ci-solver /usr/local/bin/
