$:.unshift(File.dirname(__FILE__)) unless
  $:.include?(File.dirname(__FILE__)) || $:.include?(File.expand_path(File.dirname(__FILE__)))

## Load PoolParty
pwd = File.join(File.dirname(__FILE__), "poolparty")

# Load the required files
# If there is an init file, load that, otherwise
# require all the files in each directory
%w(core modules pool).each do |dir|  
  Dir["#{pwd}/#{dir}"].each do |dir|
    begin
      require File.join(dir, "init")
    rescue LoadError => e
      Dir["#{pwd}/#{File.basename(dir)}/**"].each do |file|
        require File.join(dir, File.basename(file))
      end
    end
  end
end

module PoolParty
  
end