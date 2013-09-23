#!/bin/bash

if ! [ $(id -u) = 0 ]; then
echo "You must be root to do this."
  exit 1
fi

if [ $(arch) == x86_64 ]; then
	cp bin/sisor_x86_64 bin/sisor_loader;
elif [ $(arch) == i686 ]; then
	cp bin/sisor_i686 bin/sisor_loader;
else
	exit
fi

mkdir -p /opt/sisor/
chmod 755 ./bin/*

cp bin/sisor_loader /opt/sisor/
cp sisor.lisp /opt/sisor/
cp bin/sisor /opt/sisor/
cp -R images/ /opt/sisor/

mkdir -p /usr/share/applications/
chmod +x sisor.desktop
cp sisor.desktop /usr/share/applications/

echo "Success! Sisor was installed in '/opt/sisor'!"
