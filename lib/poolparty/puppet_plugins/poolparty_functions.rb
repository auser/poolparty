require "rubygems"
require "poolparty"

module Puppet::Parser::Functions
  newfunction(:active_node_names) do |args|
    output = []
    active_node_list.each do |line|
      output << line.split(/\t/)[0]
    end
    output
  end
  newfunction(:active_node_ips) do |args|
    output = []
    active_node_list.each do |line|
      output << line.split(/\t/)[1]
    end
    output
  end
  newfunction(:active_node_list) do |args|
    ret = %x[. /etc/profile && cloud-list]
    ret.split(/\n/).map {|a| a.chomp}.reject {|a| a.nil? || a.empty? || a =~ /\*\*/}
  end
end