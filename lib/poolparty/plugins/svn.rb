module PoolParty    
  class Svn
        
    define_resource(:svn) do
      
      def has_svnpath(opts={})
        call_custom_function <<-EOE
        svnserve { #{opts[:name]}:
          source => "#{opts[:source]}",
          path => "#{opts[:path]}",
          user => "#{opts[:user] || false}",
          password => "#{opts[:password] || ""}"
        }
        EOE
      end
      
      custom_function <<-EOF
      # Serve subversion-based code from a local location.  The job of this
      # module is to check the data out from subversion and keep it up to
      # date, especially useful for providing data to your Puppet server.
      define svnserve($source, $path, $user = false, $password = false) {
          file { $path:
              ensure => directory,
              owner => root,
              group => root
          }
          $svncmd = $user ? {
              false => "/usr/bin/svn co --non-interactive $source/$name .",
              default => "/usr/bin/svn co --non-interactive --username $user --password '$password' $source/$name ."
          }   
          exec { "svnco-$name":
              command => $svncmd,
              cwd => $path,
              require => File[$path],
              creates => "$path/.svn"
          }
          exec { "svnupdate-$name":
              command => "/usr/bin/svn update",
              require => Exec["svnco-$name"],
              onlyif => '/usr/bin/svn status -u --non-interactive | /bin/grep "\*"',
              cwd => $path
          }
      }
      EOF
    end
    
  end
end