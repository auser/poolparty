module PoolParty
  class Dependencies
    
    def self.dependencies
      @dependencies ||= ["#{::File.dirname(__FILE__)}/../../vendor/dependencies/cache"]
    end
    
    def self.package(file)
      ::Tar.open(file, File::CREAT | File::WRONLY, 0644, Tar::GNU) do |tar|
        dependencies.each do |dep|
          if ::File.file? dep
            tar.append_file(dep)
          elsif ::File.directory? dep
            Dir["#{dep}/*"].each do |file|
              tar.append_file(file) if ::File.file? file
            end
          end
        end
      end      
    end
    
    def self.gems(gem_list, gem_location)
      require 'rubygems/dependency_installer'
      
      cache_dir = "#{gem_location}/cache"
      ::FileUtils.mkdir_p cache_dir rescue nil unless File.exist? cache_dir

      gem_list.each do |g|
        di = Gem::DependencyInstaller.new
        spec, url = di.find_spec_by_name_and_version(g).first
        begin
          vputs "Downloading #{g} from github (#{spec.version} - #{spec.full_name})"
          Gem::RemoteFetcher.fetcher.download spec, "http://gems.github.com", gem_location
        rescue Exception => e
          vputs "Downloading #{g} from rubyforge because #{e}"
          Gem::RemoteFetcher.fetcher.download spec, url, gem_location
        end        
      end
    end
    
    def self.packages(package_list, package_location)
      package_list.each do |package|
        puts "downloading #{package} to #{package_location}/#{package.split('/').last}"
        `curl -L #{package} > #{package_location}/#{package.split('/').last}`        
      end
    end
    
  end
end