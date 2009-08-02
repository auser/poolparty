#!/bin/sh

#Simple test script for poolparty examples
# TODO: investiage using TAP test anything protocol for this
# http://en.wikipedia.org/wiki/Test_Anything_Protocol
# http://github.com/chneukirchen/knock

#You must run this file from within the examples/ directory

. ./knock.sh

cld='/eucalyptus.rb'

# result=../bin/cloud-list -c $cld
ok "../bin/cloud-list -c $cld -n sample|grep -v i-*"
#check that its zero
# 
# ./bin/cloud-start -c $cld
ok "../bin/cloud-start -c $cld |grep  192*"
 
ok "../bin/cloud-list -c $cld -n sample|grep  i-*"
# #check that it is 1
 
# ./bin/cloud-expand -c $cld
# #assert_equal 2, nodes.size
 
# ./bin/cloud-contract -c $cld
# #assert_equal 1, nodes.size
 
ok "../bin/cloud-terminate -c $cld"
# #assert_equal 0, nodes.size