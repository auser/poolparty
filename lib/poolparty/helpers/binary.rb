require "ftools"
module PoolParty
  
  # Load a file that contains a pool into memory
  def load_pool(filename)

    unless filename && ::File.readable?(filename)
      puts "Please specify your cloud with -s, move it to ./pool.spec or in your POOL_SPEC environment variable"
      exit(1)
    else
      PoolParty::Script.inflate(open(filename).read, filename)
    end
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
            ENV["POOL_SPEC"],            
            "#{Base.storage_directory}/#{Base.default_specfile_name}",
            "#{Base.default_project_specfile_name}"
        ].reject {|a| a.nil?}.reject do |f|
          f unless ::File.readable?(f)
        end.first
      end
      # Daemonize the process
      def daemonize(&block)
        vputs "Daemonizing..."
        trap("CHLD") {Process.wait(-1, Process::WNOHANG)}        
        fork do
          Signal.trap('HUP', 'IGNORE') # Don't die upon logout
          File.open("/dev/null", "r+") do |devnull|
            $stdout.reopen(devnull)
            $stderr.reopen(devnull)
            $stdin.reopen(devnull) unless @use_stdin
          end
          block.call if block
        end
      end
      
    end
    
  end
end