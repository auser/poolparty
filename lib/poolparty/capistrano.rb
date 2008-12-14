module PoolParty
  module Capistrano
    
    def set_cloud(cld=nil)
      raise unless cld
      @cloud = cld
    end
    
    def set_poolparty_roles
      returning Array.new do |arr|
        arr << "role 'master.#{@cloud.name}'.to_sym, '#{@cloud.master.ip}'"
        arr << "role :master, '#{@cloud.master.ip}'"
        arr << "role :slaves, '#{@cloud.nonmaster_nonterminated_instances.map{|a| a.ip}.join('", "')}'" if @cloud.nonmaster_nonterminated_instances.size > 0
      end.join("\n")
    end
    
  end
end