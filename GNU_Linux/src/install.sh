#!/bin/bash

die() { echo -e "\033[1m$@\033[0m" 1>&2 ; exit 1; }

if ! [ $(id -u) = 0 ]; then
echo "You must be root to do this."
  exit 1
fi

echo -n "Checking for ImageMagick... "
if !((which convert) > /dev/null);
then
echo -e "\033[1mERROR!\n\nImageMagick is not installed! Please install ImageMagick.\033[0m"
  exit 1
fi
echo "OK"

echo -n "Checking for Sqlite3... "
if !((which sqlite3) > /dev/null);
then
echo -e "\033[1mERROR!\n\nSqlite3 is not installed! Please install Sqlite3.\033[0m"
  exit 1
fi
echo "OK"

if [ $(arch) == x86_64 ]; then
	cp bin/sisor_x86_64 bin/sisor_loader;
elif [ $(arch) == i686 ]; then
	cp bin/sisor_i686 bin/sisor_loader;
else
	die "Unsupported architecture!"
fi

mkdir -p /opt/sisor/
chmod 755 ./bin/*
chmod 755 uninstall.sh

cp bin/sisor_loader /opt/sisor/
cp sisor.lisp /opt/sisor/
cp bin/sisor /opt/sisor/
cp -R images/ /opt/sisor/
cp uninstall.sh /opt/sisor/

mkdir -p /usr/share/applications/
chmod +x sisor.desktop
cp sisor.desktop /usr/share/applications/

echo -e "\n\033[1mSuccess! Sisor was installed in '/opt/sisor'!"
echo -e "You can launch it from the menu/dash.\033[0m"
