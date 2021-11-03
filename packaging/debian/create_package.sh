#!/bin/sh -e

# only execute anything if either
# - running under orb with package = builder
# - not running under opam at all
if [ "$ORB_BUILDING_PACKAGE" != "builder" -a "$OPAM_PACKAGE_NAME" != "" ]; then
    exit 0;
fi

basedir=$(realpath "$(dirname "$0")"/../..)
bdir=$basedir/_build/install/default/bin
tmpd=$basedir/_build/stage
rootdir=$tmpd/rootdir
bindir=$rootdir/usr/bin
systemddir=$rootdir/usr/lib/systemd/system
confdir=$rootdir/etc/builder
debiandir=$rootdir/DEBIAN

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$bindir" "$debiandir" "$systemddir" "$confdir"

# stage app binaries
install $bdir/builder-server $bindir/builder-server
install $bdir/builder-client $bindir/builder-client
install $bdir/builder-worker $bindir/builder-worker
install $bdir/builder-inspect $bindir/builder-inspect

# install service scripts
install -m 0644 $basedir/Linux/builder.service $systemddir/builder.service
install -m 0644 $basedir/Linux/builder-worker.service $systemddir/builder-worker.service

# install templates
install -m 0644 $basedir/packaging/orb-build.template.ubuntu-20.04 $confdir/orb-build.template.ubuntu-20.04
install -m 0644 $basedir/packaging/orb-build.template.freebsd $confdir/orb-build.template.freebsd

# install debian metadata
install -m 0644 $basedir/packaging/debian/control $debiandir/control
install -m 0644 $basedir/packaging/debian/changelog $debiandir/changelog
install -m 0644 $basedir/packaging/debian/copyright $debiandir/copyright
install -m 0644 $basedir/packaging/debian/conffiles $debiandir/conffiles
install $basedir/packaging/debian/postinst $debiandir/postinst

dpkg-deb --build $rootdir $basedir/builder.deb
echo 'bin: [ "builder.deb" ]' > $basedir/builder.install
echo 'doc: [ "README.md" ]' >> $basedir/builder.install
