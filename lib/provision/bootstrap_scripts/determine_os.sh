#!/bin/sh -x

if [ `uname -r | egrep '(6.2-RELEASE|6.1-RELEASE|5.5-RELEASE|6.1-STABLE|5.4-RELEASE|6.0-RELEASE|5.3-RELEASE|4.10-RELEASE|4.11-RELEASE)'` ]; then 
  echo "FreeBSD"
elif [ `uname | egrep 'arwin'` ]; then
  echo "OSX"
elif [ -f /etc/lsb-release ]; then
  echo "Ubuntu"
elif [ -f /etc/redhat-release ]; then
	os[1]='Fedora'
	os[2]='CentOS'
	os[3]='Redhat'
  for i in `seq 1 3`; do
    if [ "$(egrep "${os[$i]}" /etc/redhat-release)" ]; then
      echo "${os[$i]}"
    fi
  done
elif [ -f /etc/debian_version ]; then
  echo "Debian"
fi
exit 0