module PoolParty
  
  # Allows you to specify a gem to be installed
  # You can optionally pass a :download_url if you want a specific gem or version installed
  # example:
  #   has_gem_package :name => 'rubot',  :download_url => 'http://rubyforge.org/frs/download.php/35089/rubot-base-0.0.1.gem'

=begin rdoc
== Gempackage

Gempackages describe distributed gems on the cloud. The gem is only downloaded once and then hosted on the master if a download url is given. 

== Usage

  has_gempackage(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> The gem name
* <tt>download_url</tt> The location of the gem to download and server across the instances
* <tt>version</tt> The gem version requirement (optional and useless if download_url is given)
* <tt>source</tt> The gem source (optional and useless matter if download_url is given)

== Examples
  has_gempackage(:name => 'rake', 
    :download_url => 'http://rubyforge.org/frs/download.php/43954/rake-0.8.3.gem')
=end
  
  class GempackageResource
    
    virtual_resource(:gem_package) do
      
      dsl_methods :name,            # Name of the gem
                  :download_url,    # Url to download the gem. If not set, it will try to grab the latest gem available on gems.github.com or rubyforge
                  :version,         # Version of the gem required
                  :source           # If source is available, it will use this as the gem source
      
      def loaded(opts={}, &block)
        if download_url?
          has_exec(
            :name => "download-#{name}", 
            :cwd => Default.remote_storage_path, 
            :command => "wget #{download_url} -O #{name}.gem", 
            :if_not => "test -f #{Default.remote_storage_path}/#{name}.gem"
          )
          has_exec(
            :name => "install-#{name}-gem",
            :command => "gem install --no-ri --no-rdoc  #{Default.remote_storage_path}/#{name}.gem",
            :if_not => "gem list --local #{name} | grep #{name} #{"| grep #{version}" if version?}",
            :requires => "download-#{name}"
          )
        else
          has_exec(
            :name => "#{name}",
            :command  => "gem install --no-ri --no-rdoc #{"--version #{version}" if version?} #{"--source #{source}" if source?} #{name}",
            :if_not => "gem list --local #{name} | grep #{name} #{"| grep #{version}" if version?}"
          )
        end
      end
      
    end
  end
end