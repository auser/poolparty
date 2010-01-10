module CloudProviders
  class Ec2Helper < CloudProvider

    def initialize(name=cloud.proper_name, init_opts={}, &block)
      @name = name
      if name.is_a?(Hash)
        @name = name[:name]
        @init_opts = name if init_opts.empty?
      else
        @init_opts = init_opts
      end
      set_vars_from_options(init_opts)
      instance_eval &block if block
      after_initialized
    end

    def elb
      cloud.elb
    end

    def ec2
      cloud.ec2
    end

    def as
      cloud.as
    end

    def rds
      cloud.awsrds
    end

    def pool
      cloud.parent
    end

    def self.property(*names)
      names.each do |name|
        define_method name do |*args|
          instance_variable_set("@#{name}", args.first) unless args.empty?
          instance_variable_get("@#{name}")
        end
      end
    end

  end
end
