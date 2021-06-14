FROM ubuntu:20.04

COPY orb /root/orb
COPY builder-worker /root/builder-worker
COPY opam.sh /usr/bin/opam

CMD ["/root/builder-worker", "-r", "172.17.0.1:1234"]
