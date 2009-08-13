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
  /etc/init.d/hostname.sh start 2>/dev/null
fi

# Setup the .ssh directory
mkdir -p /var/poolparty/.ssh/
mkdir -p /var/poolparty/tmp

# Move into a tmp directory
cd /var/poolparty/tmp

# Add sudo access for poolparty
echo "poolparty ALL=(ALL) ALL" >> /etc/sudoers

rpm -Uvh http://download.fedora.redhat.com/pub/epel/5/i386/epel-release-5-3.noarch.rpm

# Install ruby and the build essential packages
echo "Installing ruby"
yum update
yum -y install cron
yum -y install  gcc-c++ readline-devel gcc make wget rsync #ruby ruby-devel
yum -y install git

wget http://rubyforge.org/frs/download.php/58677/ruby-enterprise-1.8.6-20090610.tar.gz
cd ruby-enterprise-1.8.6-*
./installer --auto=/usr/bin

# Install rubygems
# echo "Installing rubygems"
# wget http://rubyforge.org/frs/download.php/57643/rubygems-1.3.4.tgz
# tar -zxvf rubygems-1.3.4.tgz 
# cd rubygems-1.3.4
# ruby setup.rb --no-ri --no-rdoc
# cd ../
# rm -rf rubygems-1.3*

# Add the proper gem sources
gem source --add http://gems.github.com
gem source --add http://gems.opscode.com

echo "Installing PoolParty"
# Get the PoolParty and friends gems
gem install --no-ri --no-rdoc json
gem install --no-ri --no-rdoc net-ssh
gem install --no-ri --no-rdoc rake
gem install --no-ri --no-rdoc chef --source http://gems.opscode.com
gem install --no-ri --no-rdoc ohai --source http://gems.opscode.com
gem install --no-ri --no-rdoc auser-poolparty

echo "Done!"
touch /var/poolparty/bootstrapped