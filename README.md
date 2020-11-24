# builder - scheduling and executing jobs

This consists of two programs, a client and a server. The single server
contains a queue of jobs, which are consumed by a client. Any number of
clients can be connected to the server.

The server keeps persistent state of the job queue (so restarts / crashes are
dealt with). A client connects, provides some information about itself, and
then waits for a job. Once a job is read and accepted, it is executed by the
client. Resulting artifacts can be transferred by the client to the server.
2
The server has the ability to schedule jobs at regular intervals - similar to
crontab.

Handled and unhandled error conditions:
- client execution fails (timeout, restart, killed): not handled
- client execution gets a signal: reported to server
- client can't write job data files -> failure is captured and reported
- client can't read job output -> logged to client's console (without artifacts gathered)
- client errors when submitting console output -> ignored
- client errors when submitting build artifacts -> retry
- there's no explicit ACK for packets

Left to do:
- client should queue up console output on server connection failure (and continue reading output at the same time)
- client should inform when data passed to it and artifacts overlap (name / checksum)
- client could sandbox the executed script a bit more (maybe?)
- client should have a timeout for the script to be executed
- separate stdout and stderr to be sent to the server?
- UI
- retrieve artifacts even if execution failed
- the running jobs are not stored onto disk, which may result in unexpected behaviour
- should instead once a job is scheduled the uuid and job information being dumped to disk already (also avoids dummy job dump)
- directory traversals (server folds over output directory and collects files)
