module PoolParty    
  module Cloud
    def cloud(name=:main, &block)
      clouds.has_key?(name) ? clouds[name] : (clouds[name] = Cloud.new(name, &block))
    end

    def clouds
      @@clouds ||= {}
    end    
    
    class Cloud
      attr_reader :name, :options, :templates
      include MethodMissingSugar
      
      def initialize(name, opts={}, &block)
        @name = name
        instance_eval &block
      end
      
      def options(h={})
        @options ||= {
          :minimum_instances => 2,
          :maximum_instances => 4,
          :access_key => ENV["AWS_ACCESS_KEY"],
          :secret_access_key => ENV["AWS_SECRET_ACCESS"],
          :ec2_dir => ENV["EC2_HOME"],
          :keypair => (ENV["KEYPAIR_NAME"].nil? || ENV["KEYPAIR_NAME"].empty?) ? File.basename(`pwd`).strip : ENV["KEYPAIR_NAME"],
          :ami => 'ami-44bd592d',
          :polling_time => "30.seconds"
        }.merge(h).to_os
      end
      
      alias_method :configure, :options
      
      def templates(*args)
         returning (@templates ||= []) do |templates|
           templates << args
         end
      end
            
      def output(*args)
        returning (@output ||= []) do |output|
          args.each do |line|
            output << line
          end
        end
      end
      
    end
  end  
end