#!/bin/sh

: ${JAILCONF="jail.conf"}
: ${POOL=$(zpool list -H -o name | head -1)}
: ${SNAPSHOT=${POOL}/poudriere/jails/13-0-release@clean}

case "$1" in
    network_start)
        ifconfig lo1 create
        ifconfig lo1 name orb
        ifconfig orb alias 127.0.2.1/24
        ;;
    network_stop)
        ifconfig orb name lo1
        ifconfig lo1 destroy
        ;;
    build)
	echo "`date` building $2"
	zfs clone "${SNAPSHOT}" "${POOL}/jails/${2}"
	echo "`date` starting jail $2"
        cp /etc/resolv.conf /jails/${2}/etc/
	jail -f "${JAILCONF}" -c "${2}"
	echo "`date` build $2 finished"
	umount -f "/jails/${2}/dev" 2>/dev/null || true
	umount -f "/jails/${2}" 2>/dev/null || true
	zfs destroy -rf "${POOL}/jails/${2}"
	echo "`date` build $2 cleaned up"
        ;;
    *)
        echo "unsupported: provide network_start, network_stop, or build <jail>"
        ;;
esac
