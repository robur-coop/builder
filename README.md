# builder - scheduling and executing jobs

This consists of three programs, a worker, a server, and a client. The single
server contains a queue of jobs, which are consumed by a worker. Any number of
worker can be connected to the server. The client can modify the schedule:
add/remove/modify jobs, also observe a concrete job.

The server keeps persistent state of the job queue (so restarts / crashes are
dealt with). A worker connects, provides some information about itself, and
then waits for a job. Once a job is read and accepted, it is executed by the
worker. Resulting artifacts can be transferred by the client to the server.

The client has the ability to schedule jobs at regular intervals - similar to
crontab - but workers are usually executed in sandboxes/ jailed environments.

Handled and unhandled error conditions:
- worker execution fails (timeout, restart, killed): not handled, but server has a timeout
- worker execution gets a signal: reported to server
- worker can't write job data files -> failure is captured and reported
- worker can't read job output -> logged to client's console (without artifacts gathered)
- worker errors when submitting console output -> exits
- worker fails communication with server -> job is ignored (the server is responsible for restarting)

A templating mechanism is available, look for `orb-build.template` as examples.
Currently FreeBSD, Debian and Ubuntu are supported, and a repository that
receives jobs is live at https://builds.robur.coop/

## FreeBSD installation

To install a ZFS snapshot for a builder jail, the poudriere utility can be used.
After setting the ZROOT in /usr/local/etc/poudriere.conf, you can setup your
jail snapshots via: `poudriere jail -c -j 13-0-REL -m ftp-archive -v 13.0-RELEASE`
where the argument `-j` is the jailname, and `-v` describes the version to
download (NB: look into man poudriere-jail for further options).

A jail.conf and a shell-script to launch a jail are provided in the FreeBSD
subdirectory of this repository. Adjust the exec.start in jail.conf (and other
paths, such as path) if appropriate. In load.sh, adapt the zpool and zfs
snapshot source name.

Before starting your jails, they'll need an Internet connection for downloading
and installing packages, e.g. /etc/pf.conf:
nat pass on "wlan0" inet from "127.0.2.0/24" to any -> ("wlan")
