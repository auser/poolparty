#!/bin/sh -x

# Build the directories
# Make the /etc/poolparty directory to hold poolparty configuration
mkdir -p /etc/poolparty
mkdir -p /var/log/poolparty

# Build the user groups
groupadd -f poolparty
useradd poolparty  --home-dir /var/poolparty  --groups poolparty  --create-home

# Setup the .ssh directory
mkdir -p /var/poolparty/.ssh/

# Move into a tmp directory
cd /var/poolparty
mkdir tmp
cd tmp

# Add sudo access for poolparty
echo "poolparty ALL=(ALL) ALL" >> /etc/sudoers

# Check to make sure apt sources includes universe and multiverse
if [ ! `grep universe /etc/apt/sources.list` -eq 0 ]; then
	echo "deb http://archive.ubuntu.com/ubuntu jaunty universe" >> /etc/apt/sources.list
	echo "deb http://security.ubuntu.com/ubuntu jaunty-security universe" >> /etc/apt/sources.list
fi

if [ ! `grep multiverse /etc/apt/sources.list` -eq 0 ]; then
	echo "deb http://archive.ubuntu.com/ubuntu jaunty multiverse" >> /etc/apt/sources.list
	echo "deb http://security.ubuntu.com/ubuntu jaunty-security multiverse" >> /etc/apt/sources.list
fi

# Install ruby and the build essential packages
apt-get update
apt-get install -y ruby ruby1.8-dev libopenssl-ruby1.8 build-essential wget rsync

# Install rubygems
wget http://rubyforge.org/frs/download.php/57643/rubygems-1.3.4.tgz
tar -zxvf rubygems-1.3.4.tgz 
cd rubygems-1.3.4
ruby setup.rb --no-ri --no-rdoc
ln -sfv /usr/bin/gem1.8 /usr/bin/gem
cd ../
rm -rf rubygems-1.3.1*

# Add the proper gem sources
gem source --add http://gems.github.com
gem source --add http://gems.opscode.com

# Get the PoolParty and friends gems
gem install --no-ri --no-rdoc json
gem install --no-ri --no-rdoc net-ssh
gem install --no-ri --no-rdoc rake
gem install --no-ri --no-rdoc chef
gem install --no-ri --no-rdoc ohai
gem install --no-ri --no-rdoc auser-poolparty