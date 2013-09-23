#!/bin/bash

if ! [ $(id -u) = 0 ]; then
echo "You must be root to do this."
  exit 1
fi

rm -rf /opt/sisor/ || exit;
rm /usr/share/applications/sisor.desktop || exit;

echo "Sisor was successfully uninstalled."
