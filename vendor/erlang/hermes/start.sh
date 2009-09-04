#!/bin/sh

# Make the boot file, if necessary
# cd `dirname $0`
# if [[ ! -f ebin/hermes.boot ]]; then
#     make boot
#fi

# Find the next free erlang nodename using epmd  
EXISTING_NAMES=`epmd -names`

DEP_EBINS=`find deps -type d | grep -v \/test\/ | grep ebin | grep -v .svn | grep -v .git`
INCLUDE_EBINS=`find include -type d | grep -v \/test\/ | grep ebin | grep -v .svn | grep -v .git`
CLIENT_NAME="hermes"
RELEASE=`scripts/most_recent_release boot`
APP_NAME=`basename $RELEASE .boot`
echo $APP_NAME
 
exec erl \
    -pa ebin \
    -pa $DEP_EBINS \
    -pa $INCLUDE_EBINS \
    -name $CLIENT_NAME \
    -s reloader \
    -boot $APP_NAME \
    $*