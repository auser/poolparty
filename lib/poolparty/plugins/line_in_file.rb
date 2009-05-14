module PoolParty    
=begin rdoc


== Line in File

Ensures that the line given is in the file

== Usage

  has_line_in_file('line', '/full/file/path.ext')

== Examples

  has_line_in_file("ENABLED=1", "/etc/default/haproxy")
=end

  class LineInFile
    
    plugin :line_in_file do
      dsl_methods :file, :line
      default_options(
        :line => ""
      )
      def loaded(opts={}, &block)
        has_exec "line_in_#{file}" do
          command "grep -q \'#{line.safe_quote}\' #{file} || echo \'#{line.safe_quote}\' >> #{file}"
          not_if "grep -q \'#{line.safe_quote}\' #{file}"
        end
      end
    end
    
  end
end