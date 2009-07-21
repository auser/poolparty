=begin rdoc
  Bootstrapper gets the appropriate script for the os given
=end
module Provision
  class Bootstrapper
    
    def self.bootstrap_script(os=:ubuntu)
      file = File.expand_path(File.dirname(__FILE__)/"bootstrap_scripts"/"build_#{os}.sh")
      raise StandardError.new("#{os} is not supported by PoolParty's Bootstrapper") unless File.file?(file)
      file
    end
    
    def self.configure_script(cloud, os=:ubuntu, outfile=nil)
      file = File.expand_path(File.dirname(__FILE__)/"configure_scripts"/"configure_#{os}.erb")
      raise StandardError.new("#{os} is not supported by PoolParty's Bootstrapper") unless File.file?(file)
      str = ERB.new(open(file).read).result(cloud.send(:binding))
      outfile ||= cloud.tmp_path/"var"/"poolparty"/"configure_script.sh"
      File.open(outfile, "w") {|f| f << str }
      File.expand_path(outfile)
    end
    
  end
end