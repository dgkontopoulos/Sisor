#!/bin/bash

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

if [ $(uname -p) == amd64 ]; then
	cp bin/sisor_amd64 bin/sisor_loader;
elif [ $(arch) == i386 ]; then
	cp bin/sisor_i386 bin/sisor_loader;
else
	exit
fi

mkdir -p /opt/sisor/
chmod 755 ./bin/*

cp bin/sisor_loader /opt/sisor/
cp sisor.lisp /opt/sisor/
cp bin/sisor /opt/sisor/
cp -R images/ /opt/sisor/images/

mkdir -p /usr/share/applications/
chmod +x sisor.desktop
cp sisor.desktop /usr/share/applications/

echo -e "\n\033[1mSuccess! Sisor was installed in '/opt/sisor'!"
echo -e "You can launch it from the menu.\033[0m"
