require 'optparse' 
require "poolparty"
require "poolpartycl"
# require 'rdoc/usage'
require 'ostruct'
require 'date'

module PoolParty
  class Optioner
    include Dslify
    
    attr_accessor :loaded_pool, :loaded_clouds, :loaded_pools
    
    dsl_methods :spec,
                :num,
                :cloudname,
                :poolname
                
    default_options( :verbose => false, 
                     :quiet => false)
    
    def initialize(args=[], opts={}, &block)
      boolean_args << opts[:boolean_args] if opts.has_key?(:boolean_args)
      
      @arguments = parse_args(args)
      @extra_help = opts.has_key?(:extra_help) ? opts[:extra_help] : ""
      @abstract = opts.has_key?(:abstract) ? opts[:abstract] : false
      @load_pools = opts.has_key?(:load_pools) ? opts[:load_pools] : !@abstract
      @parse_options = opts.has_key?(:parse_options) ? opts[:parse_options] : true
      @command = opts.has_key?(:command) ? opts[:command] : false
      
      parse_options(&block) if @parse_options
      self
    end
    def daemonizeable
      @opts.on('-D', '--daemonize', 'Daemonize starting the cloud')    { self.daemon true }
    end
    def cloudnames
      @opts.on('-n cloudname', '--name name', 'Start cloud by this name')    { |c| self.cloudname c }
    end 
    def unflagged_args
      @unflagged_args ||= []
    end
    def flagged_args
      @flagged_args ||= []
    end
    def boolean_args
      @boolean_args ||= ['-V', '-h', '-t', '-v', '--debug']
    end
    def add_args(*syms)
      syms.each {|sym| self.class.create_method_on(self, sym) }
    end
    
    # Break ARGV into 2 arrays, one for flagged options one for unflagged
    # For example the "command -v -i 1 five six -x"
    # becomes ['-v', '-i', 1, '-x'] and ['five', 'six']
    # Boolean options, such as -v, must be specified in the optioner definition
    def parse_args(args=[])
      i=0
      while i < args.length
        if boolean_args.include?(args[i])
          flagged_args << args[i]
        else
          if args[i][0].chr == "-"
            flagged_args << args[i]
            flagged_args << args[i+1] if (args[i+1] and !args[i+1].nil?)
            i+=1
          else
            unflagged_args << args[i]
          end
        end
        i+=1
      end
      args
    end
    
    # def parent
    #   self
    # end
    
    def parent
      self
    end
    
    def parse_options(&blk)
      self.spec nil
      self.num nil
      
      progname = $0.include?("-") ? "#{::File.basename($0[/(\w+)-/, 1])} #{::File.basename($0[/-(.*)/, 1])}" : ::File.basename($0)
      @opts = OptionParser.new
      @opts.extend(Dslify)
      @opts.banner = "Usage: #{progname} #{@abstract ? "[command] " : ""}[options]"

      @opts.separator ""
      
      unless @abstract
        @opts.separator "Options:"
        
        @opts.on('-v', '--verbose', 'Be verbose') { self.verbose true }  
        @opts.on('-d', "--debug", "Debug setting") { self.debugging true }
        @opts.on('-s [file]', '--spec-file [file]', 'Set the spec file') { |file| self.spec file.chomp }
        @opts.on('-t', '--test', 'Testing mode') { self.testing true }
        
        blk.call(@opts, self) if blk
      end
      
      @opts.on('-V', '--version', 'Display the version')    { output_version ; exit 0 }
      @opts.on_tail("-h", "--help", "Show this message") do
        puts @opts
        puts @extra_help
        exit
      end
      
      @opts.parse(@arguments.dup)
      
      process_options
      output_options if verbose? && verbose
      
      if @load_pools
        @loaded_pool = load_pool( spec? ? spec : Binary.get_existing_spec_location)
        @loaded_clouds = extract_cloud_from_options(self)
        @loaded_pools = extract_pool_from_options(self)

        reject_junk_options!
        raise CloudNotFoundException.new("Please specify your cloud with -s, move it to ./clouds.rb or in your POOL_SPEC environment variable") unless loaded_clouds && !loaded_clouds.empty?
        loaded_pools.each do |pl|
          pl.dsl_options.merge!(self.options)
        end
        loaded_clouds.each do |cl|
          cl.dsl_options.merge!(self.options)
        end
      end
    end
    
    def reject_junk_options!
      %w(loaded_pool cloudname extract_pool_from_options).each do |opt|
        @dsl_options.delete(opt.to_sym)
      end
    end
    
    def process_options
    end
    
    def output_options
      puts ""
    end
        
    def output_version
      puts ::PoolParty::Version
    end
    
    def output_options      
    end
    
  end
  
  def extract_cloud_from_options(o)
    o.cloudname? ? [cloud(o.cloudname.downcase.to_sym)] : clouds.collect {|n,cl| cl}
  end
  
  def extract_pool_from_options(o)
    o.poolname? ? [pool(o.poolname.downcase.to_sym)] : pools.collect {|n,pl| pl}
  end
end