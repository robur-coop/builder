#!/bin/sh -e

# only execute anything if either
# - running under orb with package = builder
# - not running under opam at all
if [ "$ORB_BUILDING_PACKAGE" != "builder" -a "$OPAMVAR_PACKAGE_NAME" != "" ]; then
    exit 0;
fi

basedir=$(realpath "$(dirname "$0")"/../..)
pdir=$basedir/packaging/FreeBSD
bdir=$basedir/_build/install/default/bin
tmpd=$basedir/_build/stage
manifest=$tmpd/+MANIFEST
rootdir=$tmpd/rootdir
sbindir=$rootdir/usr/local/sbin
rcdir=$rootdir/usr/local/etc/rc.d
libexecdir=$rootdir/usr/local/libexec

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$rootdir"/usr/local/sbin \
         "$rootdir"/usr/local/libexec \
         "$rootdir"/usr/local/etc/rc.d

# stage service scripts
install -U $pdir/rc.d/builder $rcdir/builder

# stage app binaries
install -U $bdir/builder-server $libexecdir/builder-server
install -U $bdir/builder-worker $libexecdir/builder-worker

install -U $bdir/builder-client $sbindir/builder-client

# create +MANIFEST
flatsize=$(find "$rootdir" -type f -exec stat -f %z {} + |
               awk 'BEGIN {s=0} {s+=$1} END {print s}')

sed -e "s:%%FLATSIZE%%:${flatsize}:" "$pdir/MANIFEST" > "$manifest"

{
    printf '\nfiles {\n'
    find "$rootdir" -type f -exec sha256 -r {} + | sort |
        awk '{print "    " $2 ": \"" $1 "\"," }'
    find "$rootdir" -type l | sort |
        awk "{print \"    \"\$1 \": -,\"}"
    printf '}\n'
} | sed -e "s:${rootdir}::" >> "$manifest"

export SOURCE_DATE_EPOCH=$(git log -1 --pretty=format:%ct)
pkg create -r "$rootdir" -M "$manifest" -o $basedir/
mv $basedir/builder-*.txz $basedir/builder.txz
echo 'bin: [ "builder.txz" ]' > $basedir/builder.install
