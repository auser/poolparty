module PoolParty    
  class Gem
        
    define_resource(:gem_package) do
      
      def has_gem_package(opts={})
        call_function <<-EOE
        gem_package { #{opts[:name]}:
          source => "#{opts[:source] || "http://gems.github.com" }",
          name => "#{opts[:name]}"
        }
        EOE
      end
      
      custom_function <<-EOF
define gem_package ($source = "http://gems") {
  include ruby
  exec { "gem-package-$name":
    path => "/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/var/lib/gems/1.8/bin",
    cwd => "/tmp",
    command => "gem install --source $source $name",
    unless => "gem list --local $name | grep \"$name\""
  }
}
      EOF
    end
    
  end
end