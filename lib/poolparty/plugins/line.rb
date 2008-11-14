module PoolParty    
  class Line
        
    define_resource(:line_in_file) do
      # Should refactor
      def has_line_in_file(line="line_in_file", file="file", opts={})
        call_function <<-EOE
        line {
          '#{file}_line' :
            file => '#{file}',
            line => '#{line}',
            #{opts.flush_out("\t", ",")}
            ensure => 'present'
        }
        EOE
      end
      
      def append_if_no_such_line(line="line", file="", refreshonly='false')
        call_function <<-EOE
        append_if_no_such_line {
          '#{file}' :
            file => '#{file}',
            line => '#{line}',
            refreshonly => #{refreshonly}
        }
        EOE
      end
      
      def delete_lines_from(file="", pattern=//, opts={})
        call_function <<-EOE
        delete_lines {
          '#{file}' :
            file => '#{file}',
            #{opts.flush_out("\t", ",")}
            pattern => '#{pattern}'
        }
        EOE
      end
      
                    
      custom_function <<-EOF
      define line($file, $line, $ensure = 'present', $notify=[]) {
        case $ensure {
          default: { err ( "unknown ensure value ${ensure}" ) }
          present: {
            exec {
              "/usr/bin/env echo '${line}' >> '${file}'": unless => "/usr/bin/env grep -qFx '${line}' '${file}'",
               notify => $notify
              }
            }
          absent: {
            exec {
              "/usr/bin/env sed -i '' -e '/^${line}\$/d' '${file}'": onlyif => "/usr/bin/env grep -qFx '${line}' '${file}'",
              notify => $notify
            }
          }
        }
      }
      define append_if_no_such_line($file, $line, $refreshonly = 'false') {
         exec { "/bin/echo '$line' >> '$file'":
            unless      => "/bin/grep -Fxqe '$line' '$file'",
            path        => "/bin",
            refreshonly => $refreshonly,
         }
      }

      define delete_lines($file, $pattern) {
         exec { "sed -i -r -e '/$pattern/d' $file":
            path   => "/bin",
            onlyif => "/bin/grep -E '$pattern' '$file'",
         }
      }
      EOF
    end
    
  end
end