module PoolParty    
  module Plugin
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
* <tt>name</tt> The location of the ssh key

== Examples

  has_sshkey(:key => "ABIGLONGSTRINGOFDIGETS", :name => "/root/.ssh/key_file")
=end
    
    class Sshkey < Plugin
      
      dsl_methods(:key,
                  :keypath,
                  :content,
                  :name)

      default_options(:type => 'rsa', :mode => "600")
                  
      def initialize(opts={}, extra_opts={}, &block)
        super(opts, extra_opts, &block)
        @key = Key.new(keypath ? keypath : nil)
        self.key = @key.content
      end

      def loaded(opts={}, &block)
        has_directory(::File.dirname(opts[:name]))
        has_file(:name => opts[:name], :content => opts[:content], :mode => opts[:mode])
      end

      
    end
    
  end
end
