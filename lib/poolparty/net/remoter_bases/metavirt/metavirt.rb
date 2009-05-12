=begin rdoc
 The metavirt remoter base send all commands to a RESTful HTTP server that implements the commands
  
=end
require 'restclient'

module PoolParty
  module Remote
    class Metavirt < Remote::RemoterBase
      include Dslify


      default_options(
        # :machine_image => 'ubuntu-kvm',
        :keypair       => nil,  #TODO lambda{ keypair.to_s  },
        :public_key    => nil, #TODO lambda{ keypair.public_key.to_s  }
        :server_config => {:content_type =>'application/json', 
                           :accept      => 'application/json',
                           :host        => 'http://localhost',
                           :port        => '3000'},
        :vmx_files     => []
      ) 
      
      attr_accessor :id, :rank
      def key
        keypair.to_s
      end
      
      def initialize(par, opts={}, &block)
        dsl_options opts
        instance_eval &block if block
        dsl_options[:cloud] = par.name
        setup_server opts
        puts "MetaVirt setup with: #{dsl_options.inspect}"
        
        super(par, &block)
      end
      
      private
      def setup_server(opts)
        server_config.merge!(opts[:server_config] || {})
        puts 'server config='
        p uri = "#{server_config.delete(:host)}:#{server_config.delete(:port)}"
        @server = RestClient::Resource.new( uri, server_config)
      end

      public
      def self.launch_new_instance!(o={})
        new_instance(o).launch_new_instance!
      end
      def launch_new_instance!(o={})
        useable_opts = Hash.new
        options.keys.each {|k| useable_opts[k]=o[k]}
        result = JSON.parse(@server['/run-instance'].put(options.to_json)).symbolize_keys
        @id = result['id']
        result
      end
      # Terminate an instance by id
      def self.terminate_instance!(o={})
        new(nil, o).terminate_instance!
      end
      def terminate_instance!(o={})
        raise "id or instance_id must be set before calling describe_instace" if !id(o)
        @server["/instance/#{id(o)}"].delete
      end

      # Describe an instance's status, must pass :vmx_file in the options
      def self.describe_instance(o={})
        new_instance(o).describe_instance
      end
      def describe_instance(o={})
        raise "id or instance_id must be set before calling describe_instace" if !id(o)
        JSON.parse(@server["/instance/#{id(o)}"].get).symbolize_keys!
      end

      def self.describe_instances(o={})
        new_instance(o).describe_instances
      end
      def describe_instances(o={})
        JSON.parse( @server["/instances/"].get ).collect{|i| i.symbolize_keys}
      end
      
      # TODO: Rename and modularize the @inst.status =~ /pending/ so that it works on all 
       # remoter_bases
       def launch_instance!(o={}, &block)
         @inst = launch_new_instance!( o )
         sleep 2
         cloud.dputs "#{cloud.name} launched instance checking for ip..."         
         # Wait for 10 minutes for the instance to gain an ip if it doesn't already have one
         5.times do |i|
           @inst = describe_instance
           return @inst if @inst[:status] && @inst[:status] == 'running'
           print '.'
           sleep 1
         end
         cloud.dputs "Found an ip"
         cloud.dputs "#{@cloud.name} Launched instance #{@inst[:ip]}"
         cloud.dputs "   waiting for it to respond"
         # Try for 10 minutes to pint port 22 
         500.times do |i|
           print "."
           if (@inst[:public_ip] && ping_port(@inst[:public_ip], 22)) 
             #TODO make work with internal_ip|| (@inst[:internal_ip] && ping_port(@inst[:internal_ip], 22 ))
             cloud.started_instance = @inst
             cloud.call_after_launch_instance_callbacks(@inst)
             block.call(@inst) if block
             return @inst
           end
           sleep(2)
         end
         raise "Instance not responding at #{@inst.public_ip}"
       end
      
      private
      def id(o={})
        @id || o[:id] || options[:id] || o[:instance_id] || options[:instance_id]
      end
      
    end
  end
end
