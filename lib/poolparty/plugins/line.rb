module PoolParty    
  class Line
        
    define_resource(:line_in_file) do
      # Should refactor
      def has_line_in_file(line="line_in_file", file="file", ensures='present', notify="[]")
        call_function <<-EOE
        line {
          '#{file}' :
            file => '#{file}',
            line => '#{line}',
            ensure => #{ensures},
            notify => #{notify}
        }
        EOE
      end
                    
      custom_function <<-EOF
      define line($file, $line, $ensure = 'present', $notify=[]) {
        case $ensure {
          default: { err ( "unknown ensure value ${ensure}" ) }
          present: {
            exec {
              "/bin/echo '${line}' >> '${file}'": unless => "/usr/bin/grep -qFx '${line}' '${file}'",
               notify => $notify
              }
            }
          absent: {
            exec {
              "/usr/bin/sed -i '' -e '/^${line}\$/d' '${file}'": onlyif => "/usr/bin/grep -qFx '${line}' '${file}'",
              notify => $notify
            }
          }
        }
      }
      EOF
    end
    
  end
end