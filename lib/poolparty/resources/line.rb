=begin rdoc


== Line in File

Ensures that the line given is in the file

== Usage

  has_line_in_file('line', '/full/file/path.ext')

== Examples

  has_line_in_file("ENABLED=1", "/etc/default/haproxy")
=end
module PoolParty
  module Resources
    
    class Line < Resource
      
      default_options(
        :file => nil,
        :line => ""
      )
      
      def self.has_method_name
        "line_in_file"
      end
      
      def filepath
        file || name
      end
      
      def after_loaded
        opts = if exists?
          {:command => "grep -q \'#{line.safe_quote}\' #{filepath} || echo \'#{line.safe_quote}\' >> #{filepath}",
          :not_if => "grep -q \'#{line.safe_quote}\' #{filepath}"}
        else
          {:command => "cat #{filepath} | grep -v \'#{line.safe_quote}\' > temptfile && mv tempfile #{filepath}",
          :only_if => "grep -q \'#{line.safe_quote}\' #{filepath}"}
        end        
        
        opts.merge!(:name => exists? ? "line in #{filepath}" : "no line in #{filepath}")
        
        e = has_exec opts
        
        # Not incredibly pretty. 
        # {:file => [["pool_name", :reload]]}
        # TODO: Find an alternative
        e.meta_notifies = meta_notifies if meta_notifies
        e.meta_subscribes = meta_subscribes if meta_subscribes
        
        # TODO: Figure out better solution
        deps = @dependencies
        e.instance_eval do
          @dependencies = deps
        end
      end
      
      def print_to_chef
        :no_print
      end
      
    end
    
  end
end