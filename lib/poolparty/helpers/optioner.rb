require 'optparse' 
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
      
      set_default_options
      parse_options(&block) if @parse_options
      self
    end
    
    def parse_args(argv, safe=[])
      argv
    end
    
    def parent
      self
    end
    
    def set_default_options
      @options = {}
      @options[:verbose] = false
      @options[:quiet] = false
      @options[:version] = PoolParty::VERSION::STRING
    end
    
    def parse_options(&blk)
      opts = OptionParser.new 
      opts.banner = "Usage: pool [command] [options]"

      opts.separator ""
      opts.separator "Options:"
      
      opts.on('-V', '--version', 'Display the version')    { output_version ; exit 0 }
      opts.on('-v', '--verbose', 'Be verbose')    { @options[:verbose] = true }  
      opts.on('-s [file]', '--spec-file [file]', 'Set the spec file')      { |file| self.spec file }
      opts.on('-t', '--test', 'Testing mode')    { self.testing true }
                  
      blk.call(opts, self) if blk
      
      opts.on_tail("-h", "--help", "Show this message") do
        puts opts
        exit
      end
      
      opts.parse(@arguments.dup)
      
      process_options      
      output_options if verbose
    end
    def process_options
      @options[:verbose] = false if @options[:quiet]
    end
        
    def output_version
      puts version
    end
    
  end
end