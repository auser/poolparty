#!/bin/sh -x

echo "this is a custom bootstrap script"
mkdir -p /etc/poolparty
mkdir -p /var/log/poolparty
touch /var/poolparty/bootstrapped
echo "Done!"
