maintainer       "Nate Murray"
maintainer_email ""
license          "Apache 2.0"
description      "Installs/Configures ganglia"
long_description IO.read(File.join(File.dirname(__FILE__), 'README.rdoc'))
version          "0.1"

recipe "ganglia::default"
recipe "ganglia::gmetad"
recipe "ganglia::monitor_sshd"
recipe "ganglia::monitor_watson"
recipe "ganglia::web"
