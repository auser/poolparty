$LOAD_PATH<< File.dirname(__FILE__)
# Load required gems
#TODO: remove activesupport
@required_software = Array.new
%w(rubygems activesupport ftools logging resolv ruby2ruby digest/sha2 json pp).each do |lib|
  begin
    require lib
  rescue Exception => e
    @required_software << lib
  end  
end

require "#{File.dirname(__FILE__)}/poolparty/helpers/nice_printer"

unless @required_software.empty?
  @np = NicePrinter.new(45)

  # error_initializing_message.txt
  @np.header
  @np.center("Error")
  @np.left("Missing required software")
  @required_software.map {|a| @np << "  #{a}" }  
  @np << "Please install the required software"
  @np << "and try again"
  @np.empty
  @np << "Try installing #{@required_software.size == 1 ? "it" : "them"} with"
  @required_software.map {|a| @np << "  gem install #{a}" }
  @np.empty
  @np.footer
  
  @np.print
  exit(0)
end

Dir.glob(File.join(File.dirname(__FILE__),'..', 'vendor/gems/*/lib/*.rb')).each do |d|
  require d
end

t=Time.now
## Load PoolParty
module PoolParty
end

def PoolParty.require_directory(dir)
  if ::File.file?(dir)
    require dir
  else
    Dir["#{dir}/*.rb"].sort.each do |file|
       require "#{file}" if ::File.file?(file)
    end
    Dir["#{dir}/*"].sort.each do |dir|
      require_directory(dir) if ::File.directory?(dir)
    end
  end
end

#load poolparty framework in specific order
$_poolparty_load_directories = [
  "core",
  "dependencies.rb",
  "dependency_resolver/dependency_resolver_cloud_extensions.rb",
  "dependency_resolver/dependency_resolver.rb",
  "poolparty/poolparty_base_class.rb",
  "poolparty/default.rb",
  "modules",
  "exceptions",
  'poolparty/key.rb',
  "dependency_resolver",
  "aska.rb",
  "config",
  "monitors/monitor_rack",
  "capistrano.rb",
  'provisioners/provisioner_base.rb',
  'provisioners/capistrano/capistrano.rb',
  'provision',
  "extra",
  "net",
  "helpers",
  "verification",
  "poolparty/resource.rb",
  "poolparty/service.rb",
  "resources",
  "services",
  "poolparty/cloud.rb",
  "poolparty",
  "templates"
  ]
manifest_file_location = ::File.join(::File.dirname(__FILE__), '../config/manifest.pp')

if ::File.file?(manifest_file_location)
  ::File.readlines(manifest_file_location).each do |line| 
    dputs "#{::File.expand_path(line)}"
    require "#{line.gsub(/\n/, '')}"
  end
else
  $_poolparty_load_directories.each do |dir|
    PoolParty.require_directory(::File.join(::File.dirname(__FILE__),'poolparty', dir))
  end  
end

# Logging.init :debug, :info, :warn, :error, :fatal

module PoolParty
  include FileWriter
  
  def log
    @logger ||= make_new_logger rescue STDOUT
  end
  def reset!
    $pools = $clouds = $plugins = @describe_instances = nil
  end
  
  class PoolParty
  end
  
  private
  #:nodoc:#
  def make_new_logger
    FileUtils.mkdir_p ::File.dirname(Default.pool_logger_location) unless ::File.directory?(::File.dirname(Default.pool_logger_location))
    Loggable.new
  end
end

class Object
  include PoolParty
  include PoolParty::Pool
  include PoolParty::Cloud
  
  include PoolParty::DefinableResource
end

class Class
  include PoolParty::PluginModel
end

## Load PoolParty Plugins and package
module PoolParty
  %w(plugins base_packages).each do |dir|
    require_directory(::File.join(::File.dirname(__FILE__), 'poolparty', dir))
  end
end

PoolParty.reset!
dputs "duration = #{Time.now-t}"
