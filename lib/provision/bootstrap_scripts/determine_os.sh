#!/bin/sh -x

if [ `uname -r | egrep '(6.2-RELEASE|6.1-RELEASE|5.5-RELEASE|6.1-STABLE|5.4-RELEASE|6.0-RELEASE|5.3-RELEASE|4.10-RELEASE|4.11-RELEASE)'` ]; then 
  echo "freebsd"
elif [ `uname | egrep 'arwin'` ]; then
  echo "osx"
elif [ -f /etc/lsb-release ]; then
  echo "ubuntu"
elif [ -f /etc/redhat-release ]; then
	os[1]='fedora'
	os[2]='centos'
	os[3]='redhat'
  for i in `seq 1 3`; do
    if [ "$(egrep "${os[$i]}" /etc/redhat-release)" ]; then
      echo "${os[$i]}"
    fi
  done
elif [ -f /etc/debian_version ]; then
  echo "debian"
fi
exit 0