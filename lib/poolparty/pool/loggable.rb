class Loggable
  def initialize
    Logging.init :debug, :info, :warn, :error, :fatal
    
    self.class.loggers << file_logger
    file_logger.level = :warn

    self.class.loggers << stdout_logger
    stdout_logger.level = :info
  end
  def file_logger
    @file_logger ||= Logging.logger( ::File.join(Base.pool_logger_location, "pool_log.log"), logging_opts )
  end
  def stdout_logger
    @stdout_logger ||= Logging.logger(STDOUT, logging_opts.merge({:pattern => "%m\n"}))
  end
  def logging_opts
    {:pattern => "[%d] %-l : %m\n",
    :date_pattern => "%Y-%m-%d %H:%M:%S.%s"}
  end
  %w(info warn debug notice).each do |meth|
    define_method(meth.to_sym) do |*args|
      self.class.loggers.each {|l| l.send meth.to_sym, args}
    end
  end
  def self.loggers
    @loggers ||= []
  end
end