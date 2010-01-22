#
# Cookbook Name:: ganglia
# Recipe:: default

# install rrd
case node[:platform]
when "redhat","centos"
  package "apr-util-devel"
  %w{cairo-devel libxml2-devel pango-devel pango libpng-devel freetype freetype-devel libart_lgpl-devel gettext}.each {|p| package p}

  package "xorg-x11-fonts-Type1"

  # crazy hack to get it to work b/c of conflicts with centos 64/32 bit. see comment #16 http://www.cyberciti.biz/faq/howto-install-rrdtool-on-rhel-linux/
  script "64bit centos hack" do
    interpreter "sh -x"
    code <<-EOH 
    (( uname -a | grep x86_64 ))
    if [ "$?" -eq "0" ]
    then
       yum remove -y libxml2-devel.i386 libcairo-devel.i386 libpng12-devel.i386 libpng-devel.i386 glib2-devel.i386
       yum install -y pango-devel.x86_64
    fi

    # for some reason chef isn't installing these properly?
    yum install -y "freetype-devel"
    yum install -y "libpng-devel"

    EOH
  end

  script "install rrdtool" do
    interpreter "sh -x"
    code <<-EOH 
  mkdir -p /opt/local/src
  cd /opt/local/src
  wget http://oss.oetiker.ch/rrdtool/pub/rrdtool-1.3.1.tar.gz -O rrdtool-1.3.1.tar.gz
  tar -zxvf rrdtool-1.3.1.tar.gz
  cd rrdtool-1.3.1
  export PKG_CONFIG_PATH=/usr/lib/pkgconfig/
  ./configure && make && make install
    EOH
    creates "/usr/local/rrdtool-1.3.1/bin/rrdtool"
  end

  %w{rrdtool rrdcgi}.each do |r|
    link "/usr/bin/#{r}" do
      to "/usr/local/rrdtool-1.3.1/bin/#{r}"
    end
  end

  script "install libconfuse" do
    interpreter "sh -x"
    code <<-EOH 
  mkdir -p /opt/local/src
  cd /opt/local/src
  wget http://bzero.se/confuse/confuse-2.6.tar.gz -O confuse-2.6.tar.gz
  tar -xvvf confuse-2.6.tar.gz
  cd confuse-2.6
  ./configure CFLAGS=-fPIC --disable-nls # tricksy - http://www.mail-archive.com/ganglia-general@lists.sourceforge.net/msg04731.html
  make && make install
    EOH
    creates "/usr/local/lib/libconfuse.a"
  end

  script "install start-stop-daemon" do
    interpreter "sh -x"
    code <<-EOH 
  mkdir -p /opt/local/src
  cd /opt/local/src

  wget http://ftp.de.debian.org/debian/pool/main/d/dpkg/dpkg_1.14.25.tar.gz -O dpkg_1.14.25.tar.gz
  tar -xf dpkg_1.14.25.tar.gz
  cd dpkg-1.14.25
  ./configure --without-selinux --prefix=/usr
  make
  cd utils
  make
  make install
    EOH
    creates "/usr/sbin/start-stop-daemon"
  end

when "ubuntu"
  case node[:version]
  when "8.04" # need to add jaunty sources to 8.04
    execute "apt-get-update" do
      action :nothing
      command "apt-get update"
    end

    template "/etc/apt/sources.list.d/jaunty.list" do
      source "jaunty.sources.list"
      mode "0755"
      notifies :restart, resources(:execute => "apt-get-update"), :immediately
    end
  end

  %w{rrdtool build-essential librrd-dev libapr1-dev libconfuse-dev libexpat1-dev python-dev}.each {|p| package p}
end

script "install ganglia" do
  interpreter "sh -x"

  configure = "./configure --disable-python --with-librrd=/usr/local/rrdtool-1.3.1" 

  code <<-EOH 
mkdir -p /opt/local/src
cd /opt/local/src
wget http://superb-west.dl.sourceforge.net/sourceforge/ganglia/ganglia-3.1.2.tar.gz -O ganglia.tar.gz
tar -xvvf ganglia.tar.gz 
cd ganglia-3.1.2/
#{configure}
make && make install
  EOH
  creates "/usr/sbin/gmond"
  notifies(:restart, resources(:service => "apache2")) rescue nil
end

# == configure ganglia
directory "/etc/ganglia/bin/monitors" do
  action :create
  recursive true
end

template "/etc/init.d/gmond" do
  mode "0755"
  source "bin/gmond.erb"
end
service "gmond" do
  action :nothing
end

template "/etc/ganglia/gmond.conf" do
  source "gmond.conf.erb"
  mode "0755"
  notifies :restart, resources(:service => "gmond")
end

# sudoers
# /usr/sbin/adduser ves 
# ves ALL=(ALL) NOPASSWD: ALL
