=begin rdoc
 The metavirt remoter base send all commands to a RESTful HTTP server that implements the commands
  
=end
require 'restclient'

module PoolParty
  module Remote
    class Metavirt < Remote::RemoterBase
      include Dslify
      include ::PoolParty::CloudResourcer
      
      default_options(
        # :machine_image => 'ubuntu-kvm',
        # :key       => lambda {Key.new},
        # :keypair_name  => lambda {key.basename},
        # :keypair_path  => lambda {key.full_filepath},
        # :public_key    => lambda { key.public_key.to_s },
        :keypair_name    => nil,
        :keypair_path    => nil,
        :authorized_keys => nil,
        :remoter_base    => :vmrun,
        :remote_base     => nil,
        :server_config   => {}
        )
        
      def initialize(opts={}, &block)
        super
        # self.keypair_name = keypair.basename
        # # try and generate the public_key if needed
        # if self.authorized_keys.nil?
        #  self.authorized_keys = keypair.public_key  rescue nil
        # end
      end
      
      def server
        if @server
          @server
        else
          opts = {:content_type =>'application/json', 
           :accept      => 'application/json',
           :host        => 'http://localhost',
           :port        => '3000'}.merge(server_config)
          @uri = "#{opts.delete(:host)}:#{opts.delete(:port)}"
          @server = RestClient::Resource.new( @uri, opts)
        end
      end
      
      def remoter_base_options
        dsl_options[:remoter_base_options] = remote_base.dsl_options.choose do |k,v|
          v && (v.respond_to?(:empty) ? !v.empty?: true)
        end
      end
      
      def self.launch_new_instance!(o={})
        new_instance(o).launch_new_instance!
      end
      def launch_new_instance!(o={})
        opts =dsl_options.merge(:remoter_base_options=>remoter_base_options).merge(o)
        result = JSON.parse(server['/run-instance'].put(opts.to_json)).symbolize_keys!
        p result
        @id = result[:instance_id]
        puts "\nid = #{@id}\n---"
        result
      end
      # Terminate an instance by id
      def self.terminate_instance!(o={})
        new(nil, o).terminate_instance!
      end
      def terminate_instance!(o={})
        puts "ID= #{id(o)}"
        raise "id or instance_id must be set before calling describe_instace" if !id(o)
        server["/instance/#{id(o)}"].delete
      end

      # Describe an instance's status, must pass :vmx_file in the options
      def self.describe_instance(o={})
        new(o).describe_instance
      end
      def describe_instance(o={})
        raise "id or instance_id must be set before calling describe_instace" if !id(o)
        server["/instance/#{id(o)}"].get.json_parse
      end

      def self.describe_instances(o={})
        new(o).describe_instances
      end
      def describe_instances(o={})
        JSON.parse( server["/instances/"].get ).collect{|i| i.symbolize_keys!}
      end
      
      private
      def id(o={})
       @id = ( o[:id] || o[:instance_id] || @id || dsl_options[:id] || dsl_options[:instance_id] )
      end
      
    end
  end
end
