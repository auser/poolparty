module PoolParty    
  module Resources
=begin rdoc

== Ssh Key

The sshkey resource specifies an ssh key that should be distributed on all the nodes

== Usage

  has_sshkey(:key => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>key</tt> The key content for the ssh key
* <tt>target</tt> The location of the ssh key

== Examples

  has_sshkey(:key => "ABIGLONGSTRINGOFDIGETS", :target => "/root/.ssh/key_file")
=end
    
    class Sshkey < Resource
      
      dsl_methods :key,
                  :keypath

      default_options(:type => 'rsa')
                  
      def initialize(opts={}, extra_opts={}, &block)
        super(opts, extra_opts, &block)
        @key = Key.new(keypath? ? keypath : nil)
        self.key = @key.content
      end
      
      def name(i=nil)
        if i
          self.name = i
        else
          name ? self.name : ::File.basename(@key.full_filepath)
        end
      end
      
      def enctype(i=nil)
        i ? self.type = i : type
      end
      
    end
    
  end
end