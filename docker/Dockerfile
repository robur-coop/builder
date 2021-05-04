FROM ubuntu:20.04

COPY orb /root/orb
COPY builder-client /root/builder-client
COPY opam.sh /usr/bin/opam

CMD ["/root/builder-client", "-r", "172.17.0.1:1234"]
