#!/bin/bash

if ! [ $(id -u) = 0 ]; then
echo "You must be root to do this."
  exit 1
fi

rm -rf /opt/sisor/ || exit;
rm /usr/share/applications/sisor.desktop || exit;

echo -e "\033[1mSisor was successfully uninstalled.\033[0m"
