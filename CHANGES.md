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
