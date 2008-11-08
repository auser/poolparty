=begin rdoc
  A logging class to allow us to log to locations
=end
class Loggable
  def initialize    
    self.class.loggers << file_logger
    file_logger.level = :info
  end
  def file_logger
    @file_logger ||= Logging.logger( Base.pool_logger_location, logging_opts )
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