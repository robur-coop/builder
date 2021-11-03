#!/bin/sh

: ${JAILCONF="jail.conf"}
: ${POOL=$(zpool list -H -o name | head -1)}

network_start () {
    ifconfig lo1 create
    ifconfig lo1 name orb
    ifconfig orb alias 127.0.2.1/24
}

network_stop () {
    ifconfig orb name lo1
    ifconfig lo1 destroy
}

case "$1" in
    network_start)
        network_start
        ;;
    network_stop)
        network_stop
        ;;
    build)
	echo "`date` building $2"
	umount -f "/jails/${2}/dev" 2>/dev/null || true
	umount -f "/jails/${2}" 2>/dev/null || true
	zfs destroy "${POO}/jails/${2}" 2>/dev/null || true
	zfs clone "${POOL}/jail@clean" "${POOL}/jails/${2}"
	echo "`date` starting jail $2"
	jail -f "${JAILCONF}" -c "${2}"
	zfs destroy "${POOL}/jails/${2}"
	echo "`date` build $2 finished"
	;;
esac
