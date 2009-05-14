module PoolParty    
  module Resources
=begin rdoc rdoc
== Exec

Ensure a command is run on the instances

== Usage

  has_exec(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> The name of your exec. This is optional, but nice for debugging purposes
* <tt>cwd</tt> Current working directory to execute the command (optional)
* <tt>command</tt> This describes the command to run
* <tt>path</tt> The path to run the command with (optional)

== Examples
  has_exec(:name => 'start messenger', :command => 'server-start-node', :requires => get_gempackage('poolparty-latest', :onlyif => 'ps aux | grep beam | grep master')
=end
    
    class Exec < Resource
      
      dsl_methods :cwd, :creates, :command
      default_options({
        :path => ["/usr/bin:/bin:/usr/local/bin:$PATH"]
      })
      
      def present
        nil
      end
      
      def absent
        nil
      end
      
      def after_create
        dsl_options[:name] = dsl_options[:command] unless dsl_options[:name]
      end

    end
    
  end
end