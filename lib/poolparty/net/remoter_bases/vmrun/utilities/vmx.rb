=begin rdoc
  Vmx file creator
  
  Usage:
    Vmx.new({
      :name => "MyPP",
     :vmx_disk => {
       :image_size => "2G"
     },
     :base_directory => "/tmp/vmx"
    }).compile
=end
class Vmx
  include Dslify
  
  default_options(
    :name => "PoolParty",
    :base_directory => "~/Documents/Virtual\ Machines.localized"
  )
  
  def initialize(o={})
    dsl_options o
    @vmx_file = VmxFile.new o
    @vmx_disk = VmxDisk.new o    
  end
  
  def compile
    ::FileUtils.mkdir_p "#{base_directory}/#{dsl_options[:name]}"
    vmdk = @vmx_disk.compile
    @vmx_file.set("ide0:0.fileName", vmdk)
    @vmx_file.compile    
  end
end