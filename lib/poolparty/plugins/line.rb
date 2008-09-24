module PoolParty    
  class Line
        
    plugin :line do
      define_resource(:line) do        
        def has_line_in_file(line="line_in_file", file="file")
          call_function "line(#{file}, #{line})"
        end
                
        custom_function <<-EOF
        define line($file, $line, $ensure = 'present') {
          case $ensure {
            default: { err ( "unknown ensure value ${ensure}" ) }
            present: {
              exec {
                "/bin/echo '${line}' >> '${file}'":
                 unless => "/usr/bin/grep -qFx '${line}' '${file}'"
              }
            }
            absent: {
              exec {
                "/usr/bin/sed -i '' -e '/^${line}\$/d' '${file}'":
                  onlyif => "/usr/bin/grep -qFx '${line}' '${file}'"
              }
            }
          }
        }
        EOF
      end
      
    end
  end
end