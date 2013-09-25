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

mkdir -p /opt/sisor/
mkdir -p /usr/local/bin/
chmod 755 ./bin/*

cp bin/sisor_mac /opt/sisor/sisor_loader
cp sisor.lisp /opt/sisor/
cp bin/sisor /usr/local/bin/sisor
cp -R images/ /opt/sisor/images/

echo $'\033[1mSuccess! Sisor was installed in \'/opt/sisor\'!'
echo $'Launch it with \'sisor\' (without quotes).\033[0m'
