module Puppet::Parser::Functions
  newfunction(:active_node_ips, :type => :rvalue) do |args|
    output = []
    ret = %x[. /etc/profile && cloud-list]
    active_node_list = ret.split(/\n/).map {|a| a.chomp}.reject {|a| a.nil? || a.empty? || a =~ /\*\*/}    
    active_node_list.each do |line|
      output << line.split(/\t/)[1]
    end
    output
  end
end