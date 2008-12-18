require "ftools"
module PoolParty
  
  # Load a file that contains a pool into memory
  def load_pool(filename=nil)
    filename = Binary.get_existing_spec_location#Dir["#{Dir.pwd}/**/*.rb"].select {|f| ::File.basename(f) == "clouds.rb" }.first unless filename
    dputs "Using spec at #{filename}"
    
    unless filename && ::File.readable?(filename)
      puts "Please specify your cloud with -s, move it to ./clouds.rb or in your POOL_SPEC environment variable"
      exit(1)
    else
      $pool_specfile = filename
      PoolParty::Script.inflate(open(filename).read, filename)
    end
  end
  
  def print_with_nice_printer(header=nil, strs=[])
    returning NicePrinter.new do |printer|
      printer.header
      printer.center(header) if header
      strs.each {|st| printer << st if st}
      printer.footer
    end.print
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
            "#{Base.remote_storage_path}/#{Base.default_specfile_name}", 
            "#{Base.default_specfile_name}",            
            "#{Base.base_config_directory}/#{Base.default_specfile_name}",            
            Dir["#{Dir.pwd}/*/clouds.rb"],
            ENV["POOL_SPEC"],
            "#{Base.storage_directory}/#{Base.default_specfile_name}"
        ].flatten.reject {|a| a.nil?}.reject do |f|
          f unless ::File.readable?(f)
        end.first
      end
      # Daemonize the process
      def daemonize(&block)
        vputs "Daemonizing..."
        trap("CHLD") {Process.wait(-1, Process::WNOHANG)}        
        pid = fork do
          Signal.trap('HUP', 'IGNORE') # Don't die upon logout
          File.open("/dev/null", "r+") do |devnull|
            $stdout.reopen(devnull)
            $stderr.reopen(devnull)
            $stdin.reopen(devnull) unless @use_stdin
          end
          block.call if block
        end
        Process.detach(pid)
        pid
      end
      
    end
    
  end
end