class Test::Unit::TestCase
  class << self
    attr_accessor :before_each_callbacks, :before_all_callbacks, :after_each_callbacks, :after_all_callbacks, :before_should_callbacks

    # Add logic to run before the tests (i.e., a +setup+ method)
    #
    #     before do
    #       @user = User.first
    #     end
    # 
    def before(period = :each, &block)
      unless block_given?
        block  = period
        period = :each
      end

      send("before_#{period}_callbacks") << block
    end
    
    alias :setup :before
    
    # Add logic to run after the tests (i.e., a +teardown+ method)
    #
    #     after do
    #       User.delete_all
    #     end
    #
    def after(period = :each, &block)
      unless block_given?
        block  = period
        period = :each
      end

      send("after_#{period}_callbacks") << block
    end
    
    alias :teardown :after

    def gather_callbacks(callback_type, period) # :nodoc:
      callbacks = superclass.respond_to?(:gather_callbacks) ? superclass.gather_callbacks(callback_type, period) : []
      callbacks.push(*send("#{callback_type}_#{period}_callbacks"))
    end
  end

  self.before_all_callbacks    = []
  self.before_each_callbacks   = []
  self.after_each_callbacks    = []
  self.after_all_callbacks     = []
  self.before_should_callbacks = {}

  def self.inherited(child) # :nodoc:
    super
    child.before_all_callbacks    = []
    child.before_each_callbacks   = []
    child.after_each_callbacks    = []
    child.after_all_callbacks     = []
    child.before_should_callbacks = {}

    child.class_eval do
      def setup(&block)
        super
        
        unless self.class.before_should_callbacks[method_name].nil?
          instance_eval(&self.class.before_should_callbacks[method_name])
        end

        run_each_callbacks :before
      end

      def teardown
        super

        run_each_callbacks :after
      end
    end if self == Test::Unit::TestCase
  end

  def run_each_callbacks(callback_type) # :nodoc:
    self.class.gather_callbacks(callback_type, :each).each do |c| 
      c.is_a?(Proc) ? instance_eval(&c) : send(c)
    end
  end

  def run_all_callbacks(callback_type) # :nodoc:
    previous_ivars = instance_variables
    self.class.gather_callbacks(callback_type, :all).each { |c| instance_eval(&c) if c }
    (instance_variables - previous_ivars).inject({}) do |hash, ivar|
      hash.update ivar => instance_variable_get(ivar)
    end
  rescue Object => exception
    raise <<-BANG
  Error running the #{callback_type}(:all) callback for #{name}
  #{exception.class.name}: #{exception.message}
  #{exception.backtrace.join("\n")}
    BANG
  end

  def set_values_from_callbacks(values) # :nodoc:
    values.each do |name, value|
      instance_variable_set name, value
    end
  end
end
