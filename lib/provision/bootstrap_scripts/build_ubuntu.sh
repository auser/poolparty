#!/bin/sh -x

# Build the directories
# Make the /etc/poolparty directory to hold poolparty configuration
mkdir -p /etc/poolparty
mkdir -p /var/log/poolparty

# Build the user groups
if [ ! "$(egrep "poolparty" /etc/group)" ]; then
  groupadd -f poolparty
fi

if [ ! "$(egrep "poolparty" /etc/passwd)" ]; then
  useradd poolparty  --home-dir /var/poolparty  -g poolparty  --create-home
fi

# Set a hostname
if [ -z `hostname` ]; then
  hostname "poolparty"
  echo poolparty > /etc/hostname
fi

# Setup the .ssh directory
mkdir -p /var/poolparty/.ssh/
mkdir -p /var/poolparty/tmp

# Move into a tmp directory
cd /var/poolparty/tmp

# Add sudo access for poolparty
echo "poolparty ALL=(ALL) ALL" >> /etc/sudoers

# Check to make sure apt sources includes universe and multiverse
if [ ! "$(egrep "universe" /etc/apt/sources.list)" ]; then
	echo "deb http://archive.ubuntu.com/ubuntu jaunty universe" >> /etc/apt/sources.list
	echo "deb http://security.ubuntu.com/ubuntu jaunty-security universe" >> /etc/apt/sources.list
fi

if [ ! "$(egrep "multiverse" /etc/apt/sources.list)" ]; then
	echo "deb http://archive.ubuntu.com/ubuntu jaunty multiverse" >> /etc/apt/sources.list
	echo "deb http://security.ubuntu.com/ubuntu jaunty-security multiverse" >> /etc/apt/sources.list
fi

# Install ruby and the build essential packages
echo "Installing ruby"
apt-get update
apt-get install cron
apt-get install -y ruby ruby1.8-dev libopenssl-ruby1.8 build-essential wget rsync

# Install rubygems
echo "Installing rubygems"
wget http://rubyforge.org/frs/download.php/57643/rubygems-1.3.4.tgz
tar -zxvf rubygems-1.3.4.tgz 
cd rubygems-1.3.4
ruby setup.rb --no-ri --no-rdoc
ln -sfv /usr/bin/gem1.8 /usr/bin/gem
cd ../
rm -rf rubygems-1.3*

# Add the proper gem sources
gem source --add http://gems.github.com
gem source --add http://gems.opscode.com

echo "Installing PoolParty"
# Get the PoolParty and friends gems
gem install --no-ri --no-rdoc json
gem install --no-ri --no-rdoc net-ssh
gem install --no-ri --no-rdoc rake
gem install --no-ri --no-rdoc chef
gem install --no-ri --no-rdoc ohai
gem install --no-ri --no-rdoc auser-poolparty

echo "Done!"
touch /var/poolparty/bootstrapped