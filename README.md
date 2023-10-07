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

## Linux installation

Install the binary package, edit the "builder" and "builder-worker" services and
start them via systemd. Common things to add to "builder" service scripts are to
set the IP `--ip 172.17.0.1` (take care to mark "docker.service" as a dependency
in the `After=` line) and an upload URL
`--upload https://user:pass@builder-web-host/upload`.

To execute multiple workers, copy the builder-worker service multiple times. You
can configure the distribution to use in that as well.

## FreeBSD installation

To install a ZFS snapshot for a builder jail, the poudriere utility can be used.
After setting the ZROOT in /usr/local/etc/poudriere.conf, you can setup your
jail snapshots via: `poudriere jail -c -j 13-0-REL -m ftp-archive -v 13.0-RELEASE`
where the argument `-j` is the jailname, and `-v` describes the version to
download (NB: look into `man poudriere-jail` for further options).

A jail.conf and a shell-script to launch a jail are provided in the FreeBSD
subdirectory of this repository. Adjust the exec.start in jail.conf (and other
paths, such as path) if appropriate. In load.sh, adapt the zpool and zfs
snapshot source name.

Before starting your jails, they'll need an Internet connection for downloading
and installing packages, e.g. /etc/pf.conf:
`nat pass on "wlan0" inet from "127.0.2.0/24" to any -> ("wlan")`

Edit the configuration of the `builder` service and the `builder_worker` service
by editing your `/etc/rc.conf` (again, if you like multiple workers, copy the
service script):
- `cloned_interfaces="lo1"` - clone the localhost interface
- `ifconfig_lo1_name="orb"` - name it `orb`
- `ipv4_addrs_orb="127.0.2.1/24"` - configure the IP address to 127.0.2.1
- `builder_enable="YES"` - enable builder
- `builder_flags="--ip 127.0.2.1 --upload https://user:pass@builder-web-host/upload"`
- `builder_worker_enable="YES"` - enable a builder (using /usr/local/libexec/builder_worker.sh, which uses /usr/local/etc/builder/jail.conf)
