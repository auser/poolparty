=begin rdoc
  Base Provisioner responsible for forming the output on the provisioners
=end
module Provision
end

%w(bootstrapper).each do |lib|
  require "provision/#{lib}"
end