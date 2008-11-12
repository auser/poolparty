require 'rubygems'

# Load required gems
@required_software = Array.new
%w(activesupport ftools logging ruby2ruby).each do |lib|
  begin
    require lib
  rescue Exception => e
    @required_software << lib
  end  
end

unless @required_software.empty?
  @num_lines = 45
  @centered_lines = @num_lines - 4
  def wrap_cline(line)
    "* #{line.center(@centered_lines)} *"
  end
  def wrap_lline(line)
    "* #{line.ljust(@centered_lines)} *"
  end
  def header
    "*"*@num_lines
  end
  empty_line = "* #{" ".ljust(@centered_lines)} *"
  # error_initializing_message.txt
  puts header
  puts wrap_cline("Error")
  puts wrap_lline("Missing required software")
  puts @required_software.map {|a| wrap_lline("  #{a}") }  
  puts wrap_lline("Please install the required software")
  puts wrap_lline("and try again")
  puts empty_line
  puts wrap_lline("Try installing #{@required_software.size == 1 ? "it" : "them"} with")
  puts @required_software.map {|a| wrap_lline("  gem install #{a}") }  
  puts empty_line
  puts "*"*@num_lines
  exit(0)
end

# Use active supports auto load mechanism
ActiveSupport::Dependencies.load_paths << File.dirname(__FILE__)

## Load PoolParty
require "#{File.dirname(__FILE__)}/poolparty/version"

%w(core modules exceptions dependency_resolutions aska monitors net).each do |dir|
  Dir[File.dirname(__FILE__) + "/poolparty/#{dir}/**.rb"].each do |file|
    require file
  end
end

Kernel.load_p File.dirname(__FILE__) + "/poolparty/pool"
Logging.init :debug, :info, :warn, :error, :fatal

module PoolParty
  include FileWriter
  
  def logger
    @logger ||= make_new_logger
  end
  
  class PoolParty
    def initialize(spec)
      Script.inflate(spec) if spec
    end
  end
  
  private
  #:nodoc:#
  def make_new_logger
    FileUtils.mkdir_p ::File.dirname(Base.pool_logger_location) unless ::File.directory?(::File.dirname(Base.pool_logger_location))
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
%w(plugins base_packages).each do |dir|
  Dir[File.dirname(__FILE__) + "/poolparty/#{dir}/**.rb"].each do |file|
    require file
  end
end