=begin rdoc
  This is an example CloudProvider to be used as a template for implementing new CloudProviders
=end
require "#{File.dirname(__FILE__)}/ssh_instance"

module CloudProviders
  class Ssh < CloudProvider
    default_options(
      CloudProvider.default_options.merge(
        :hosts  => [] # list of hostnames, or addresses
      )
    )
    
    # By default we consider all hosts to be running
    # available_hosts represents those that we are not "running" right now
    def available_hosts
      @available_hosts ||= []
    end
    
    def running_hosts
      hosts - available_hosts
    end
    
    # Select just the instance options that are relevant to the SshInstance
    def instance_options(opts)
      i_opts = dsl_options.merge(opts)
      # i_opts = SshInstance.default_options.merge(i_opts)
      # i_opts.choose{|k,v| SshInstance.default_options.keys.include?(k)}
    end
    
    # Launch a new instance
    def run_instance(o={})
      raise StandardError.new("You have no available hosts") if available_hosts.empty?
      host = available_hosts.first
      new_host = SshInstance.new(instance_options(o.merge(:name=>host)))
      new_host.keypair
      if new_host && new_host.refresh!
        available_hosts.delete host
        new_host
      else
        raise StandardError.new("Unable to connect to host #{host}")
      end
    end
    
    # Terminate an instance by name, or the last instance
    def terminate_instance!(o={})
      thost = o[:name] || running_hosts.last
      available_hosts << thost.clone
      SshInstance.new(instance_options(o.merge(:name=>thost, :status=>'terminated')))
    end

    # Returns an instance object for the host.  You must pass the :name option
    def describe_instance(opts={:name=>nil})
      raise StandardError.new("you must provide a name") unless opts[:name]
      SshInstance.new(instance_options(o.merge(:name=>opts[:name])))
    end
    
    def describe_instances(o={})
      hosts.collect{|n| SshInstance.new(instance_options(o.merge(:name=>n)))}
    end
    
    def nodes(hsh={})
      results = running_hosts.collect{|n| SshInstance.new(instance_options(:name=>n))}
      results.select_with_hash(hsh)
    end
    
  end
end