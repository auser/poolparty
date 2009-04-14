require "ftools"
module PoolParty
  
  # Load a file that contains a pool into memory
  def load_pool(filename=nil)
    filename = filename || Binary.get_existing_spec_location
    dputs "Using spec at #{filename}"
    
    unless filename && ::File.readable?(filename)
      puts "Please specify your cloud with -s, move it to ./clouds.rb or in your POOL_SPEC environment variable"
      exit(1)
    else
      $pool_specfile = filename
      PoolParty::Pool::Pool.load_from_file filename
    end
  end
  
  def print_with_nice_printer(header=nil, strs=[], &block)
    printer = NicePrinter.new
    printer.header
    printer.center(header) if header
    yield(printer)
    strs.each {|st| printer << st if st}
    printer.footer
    printer.print
  end
  
  def pool_specfile
    $pool_specfile
  end
  
  module Binary
    
    class << self
            
      def list_binaries_for(ty="pool")
        available_binaries_for(ty).join(", ")
      end
      def available_binaries_for(ty="pool")
        Dir["#{binary_directory}/#{ty}-*"].map {|a| File.basename(a.gsub(/#{ty}-/, '')) }.sort
      end
      def binary_directory
        "#{::File.dirname(__FILE__)}/../../../bin"
      end
      # These are the locations the spec file can be before the cloud
      # aborts because it cannot load the cloud
      def get_existing_spec_location
        [          
          Dir["#{Dir.pwd}/*/clouds.rb"],
          "#{Default.remote_storage_path}/#{Default.default_specfile_name}", 
          "#{Default.default_specfile_name}",            
          "#{Default.base_config_directory}/#{Default.default_specfile_name}",
          "#{Default.poolparty_home_path}/#{Default.default_specfile_name}",          
          ENV["POOL_SPEC"]
        ].flatten.reject {|a| a.nil?}.reject do |f|
          f unless ::File.readable?(f)
        end.first
      end
      # Daemonize the process
      def daemonize(&block)
        Daemonize.daemonize(&block)
      end
      
    end
    
  end
end