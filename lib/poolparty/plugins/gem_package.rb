module PoolParty    
  class Gem
        
    define_resource(:gem_package) do
      
      def has_gem_package(opts={})
        call_function <<-EOE
gem_package { "#{opts[:package] || opts[:name]}":
  source => "#{opts[:source] || "http://gems.github.com" }",
  package => "#{opts[:package] || opts[:name]}"
}
        EOE
      end
      
      custom_function <<-EOF
      define gem_package ($source = "http://gems", $version, $package=false) {
        include ruby

        if $version {
          exec { "gem-package-$package":
            path => "/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/var/lib/gems/1.8/bin",
            cwd => "/tmp",
            command => "gem install --source $source --version \"$version\" $package",
            unless => "gem list --local $package | grep \"$package\" | grep \"$version\""
          }
        } else {
          exec { "gem-package-$package":
            path => "/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/var/lib/gems/1.8/bin",
            cwd => "/tmp",
            command => "gem install --source $source $package",
            unless => "gem list --local $package | grep \"$package\"
          }
        }
      }
      EOF
    end
    
  end
end