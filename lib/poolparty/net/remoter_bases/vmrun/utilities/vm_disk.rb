class VmxDisk
  include Dslify
  
  def initialize(o={})
    dsl_options o
  end
  
  def compile
    %x[qemu-img create -f vmdk #{base_directory}/#{name}/#{name}.vmdk #{dsl_options[:vmx_disk][:image_size]} ]
    "#{base_directory}/#{name}/#{name}.vmdk"
  end
end