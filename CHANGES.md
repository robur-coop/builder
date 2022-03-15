# v0.3.1 (2022-03-15)

* Debian postinstall: create user/group conditionally, systemd daemon-reload
* FreeBSD packaging: add user/group for builder, create /var/db/builder
* Debian packaging: set architecture to DEB_TARGET_ARCH
* FreeBSD packaging: normalize version (. instead of -)
* Debian template: install dh-exec
* Refactor builder-worker.server: split long ExecStart line, use variables
  for builder platform and docker image, pass platform to builder-worker
* Update to cmdliner 1.1.0

# v0.3.0 (2021-11-11)

* server: unstuck waiting workers when a new queue is created for a platform
* server: warn when a worker requests a job for a new platform when a template
  does not exist
* server: improve logging of workers (always prefix uuid)
* worker: kill process group when server communication fails
* worker: collect output on any exit code
* client: observe-latest has optional platform and job_name arguments
* client: execute has an optional platform argument
* Debian and FreeBSD packaging improvements

# v0.2.0 (2021-11-04)

* Client: print relative timestamp for worker output
* Client: report result of command execution
* Add platform string to jobs (supporting one server for
  multiple heterogenous workers)
* Debian packaging: add "builder" user, create /var/lib/builder,
  use 0644 for service scripts and metadata
* Worker: simplify and unify failure behaviour (exit on error)
* Revise communication protocol (breaks backwards compatibility)

# v0.1.2 (2021-10-25)

* Avoid deprecated functions of Fmt (#14 @hannesm)
* Drop rresult dependency (#14 @hannesm)

# v0.1.1 (2021-09-28)

* Create and reuse a singe happy_eyeballs state in the server (#13 @hannesm)

# v0.1.0 (2021-09-14)

* Initial public release
