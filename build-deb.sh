#!/usr/bin/env bash

set -euo pipefail

export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y --no-install-recommends \
	devscripts fakeroot haskell-debian-utils \
	libwww-perl file git

git submodule update --init haskell-download-curl

cd haskell-download-curl
origtargz -u
apt-get-build-depends -y
debuild -b -us -uc
debuild -- clean
cd ..

dpkg -i libghc-download-curl-dev_*.deb

apt-get-build-depends -y
debuild -b -us -uc
debuild -- clean
