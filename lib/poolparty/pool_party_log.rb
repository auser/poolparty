=begin rdoc
  Logger
=end
module PoolParty  
  class PoolPartyLog

    @logger = nil
    
    class << self
      attr_accessor :logger, :level, :file #:nodoc
      
      def init(path = false)
        @file = path ? File.join(path, "poolparty.log") : $stdout
        @logger = Logger.new(file)
        level = @log_level = :info
      end
      
      # Sets the level for the Logger by symbol or by command line argument.
      # Throws an ArgumentError if you feed it a bogus log level (that is not
      # one of :debug, :info, :warn, :error, :fatal or the corresponding strings)
      def level=(loglevel)
        case loglevel.to_sym
        when :debug
          @logger.level = Logger::DEBUG
        when :info
          @logger.level = Logger::INFO
        when :warn
          @logger.level = Logger::WARN
        when :error
          @logger.level = Logger::ERROR
        when :fatal
          @logger.level = Logger::FATAL
        else
          raise ArgumentError, "Log level must be one of :debug, :info, :warn, :error, or :fatal"
        end
      end
      
      def method_missing(method_symbol, *args)
        init unless @logger
        @logger.send(method_symbol, *args)
      end
      
    end # class << self


  end
end