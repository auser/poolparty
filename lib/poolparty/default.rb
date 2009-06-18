=begin rdoc
  Default class
  
  Hangs on and defines defaults for PoolParty
=end

module PoolParty
  class Default
    include Dslify
    
    default_options(
      :user => "root",
      :poolparty_home_path => "#{ENV["HOME"]}/.poolparty",
      :ec2_home => "#{ENV["HOME"]}/.poolparty",
      :poolparty_src_path => "#{::File.dirname(__FILE__)}/../../",
      :base_config_directory => "/etc/poolparty",
      :remote_storage_path => "/var/poolparty"
    )
    
    # Method missing
    def self.method_missing(m,*a,&block)
      dsl_options.include?(m) ? dsl_options[m] : super
    end
    
  end
end