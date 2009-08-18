=begin rdoc
  Bootstrapper gets the appropriate script for the os given
=end
module Provision
  class Bootstrapper
    
    # bootstrap_script
    # Get the bootstrap_script for the appropriate os located in the bootstrap_scripts directory
    # in the PoolParty provision/bootstrap_scripts directory
    # If there is no bootstrap script (of the format: build_<os>.sh), then we raise an 
    # exception to notify that the os is not yet supported
    def self.bootstrap_script(opts={})
      if opts[:filename]
        raise StandardError.new("Could not find bootstrap file #{opts[:filename]}") unless File.file?(opts[:filename])
        File.expand_path(opts[:filename])
      else
        os = opts[:os] || :ubuntu
        file = File.expand_path(File.dirname(__FILE__)/"bootstrap_scripts"/"build_#{os}.sh")
        raise StandardError.new("#{os} is not supported by PoolParty's Bootstrapper") unless File.file?(file)
        file
      end
    end
    
    # Get the determine os bootstrap script
    def self.determine_os_script
      File.expand_path(File.dirname(__FILE__)/"bootstrap_scripts"/"determine_os.sh")
    end
    
    # configure_script
    # Find the configure script that corresponds to the os given.
    # Raise an exception if the configure_script does not exist (of the format: configure_<os>.sh)
    # Use Erb to format the script with Erb and save to the given outfile (default cloud.tmp_path/var/poolparty/configure_script.sh)
    # and return the path to the file
    def self.configure_script(cloud, os=:ubuntu, outfile=nil)
      file = File.expand_path(File.dirname(__FILE__)/"configure_scripts"/"configure_#{os}.erb")
      raise StandardError.new("#{os} is not supported by PoolParty's Bootstrapper") unless File.file?(file)
      str = ERB.new(open(file).read).result(cloud.send(:binding))
      outfile ||= cloud.tmp_path/"configure_script.sh"
      File.open(outfile, "w") {|f| f << str }
      File.expand_path(outfile)
    end
    
  end
end