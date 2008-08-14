module PoolParty
  
  module Plugin
    class Plugin
      class << self
        
        
        def has_file(filelocation, opts={})
          output <<-EOF
            file { #{File.basename(filelocation, File.extname(filelocation))}:
                file  => #{filelocation},
                owner => #{opts[:owner] || "root"},
                group => #{opts[:group] || "root"}, 
                mode => #{opts[:mode] || "440"}
            }
          EOF
        end

        def has_line_in_file(line, file)
          puts "here"
          append_if_no_such_line
          output <<-EOM
          append_if_no_such_line{ #{line[-1..10]}:
                     file => "#{file}",
                     line => "#{line}"}
          EOM
        end
        
      end      
    end
    
  end
end