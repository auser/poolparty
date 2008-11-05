require 'optparse' 
require "poolparty"
require "poolpartycl"
require 'rdoc/usage'
require 'ostruct'
require 'date'

module PoolParty
  class Optioner
    include Configurable
    include MethodMissingSugar
        
    def initialize(args=[], opts={}, &block)
      @arguments = parse_args(args)
      @parse_options = opts[:parse_options] ? opts[:parse_options] : true
      @extra_help = opts.has_key?(:extra_help) ? opts[:extra_help] : ""
      @abstract = opts[:abstract] ? opts[:abstract] : false
      @command = opts[:command] ? opts[:command] : false
      
      parse_options(&block) if @parse_options
      set_default_options
      self
    end
    
    def parse_args(argv, safe=[])
      argv
    end
    
    def parent
      self
    end
    
    def set_default_options
      self.verbose false
      self.quiet false
      self.version PoolParty::VERSION::STRING
    end
    
    def parse_options(&blk)
      progname = $0.include?("-") ? "#{::File.basename($0[/(\w+)-/, 1])} #{::File.basename($0[/-(.*)/, 1])}" : ::File.basename($0)
      opts = OptionParser.new 
      opts.banner = "Usage: #{progname} #{@abstract ? "[command] " : ""}[options]"

      opts.separator ""
      opts.separator "Options:"
      
      opts.on('-V', '--version', 'Display the version')    { output_version ; exit 0 }
      opts.on('-v', '--verbose', 'Be verbose')    { self.verbose true }  
      opts.on('-s [file]', '--spec-file [file]', 'Set the spec file')      { |file| self.spec file }
      opts.on('-t', '--test', 'Testing mode')    { self.testing true }
                  
      blk.call(opts, self) if blk
      
      unless @abstract
        opts.on_tail("-h", "--help", "Show this message") do
          puts opts
          puts @extra_help
          exit
        end
      end
      
      opts.parse(@arguments.dup)
      
      process_options
      output_options if verbose
      self.loaded_pool load_pool(self.spec || Binary.get_existing_spec_location)
      
      self.loaded_clouds extract_cloud_from_options(self)
      self.loaded_pools extract_pool_from_options(self)
      
      reject_junk_options!
      raise CloudNotFoundException.new("Please specify your cloud with -s, move it to ./pool.spec or in your POOL_SPEC environment variable") unless loaded_clouds && !loaded_clouds.empty?
      loaded_clouds.each do |cl|
        cl.configure(self.options)
      end
    end
    def reject_junk_options!
      %w(loaded_pool cloudname).each do |opt|
        @options.delete(opt.to_sym)
      end
    end
    def process_options
    end
        
    def output_version
      puts version
    end
    
  end
end