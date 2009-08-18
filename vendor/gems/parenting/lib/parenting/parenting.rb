module Parenting
  module ClassMethods
  end
  
  module InstanceMethods
    def context_stack
      $context_stack ||= []
    end
    def initialize(o, &block)
      run_in_context do
        instance_eval(&block) if block
        super(&block)
      end
    end
    
    def run_in_context(&block)
      @parent = parent
      context_stack.push self
      instance_eval(&block) if block
      context_stack.pop
      head
    end

    def head
      context_stack.first
    end
    def this_context
      # @this_context ||= context_stack.last
      context_stack.last
    end
    def parent
      @parent ||= current_context[-1] == self ? current_context[-2] : current_context[-1]
    end

    def current_context
      @current_context ||= context_stack[0..depth]
    end
    def depth
      @depth ||= context_stack.size
    end
    def this
      @this ||= self
    end
    def method_missing(m,*args,&block)      
      if block
        if args.empty?
          super
        else
          inst = args[0]
          context_stack.push self
          inst.instance_eval(&block)
          context_stack.pop
        end
      else
        # if this_context.respond_to?(m)
        #   this_context.send(m,*args,&block)
        begin
          parent.send(m,*args, &block)
        rescue Exception => e
          dputs [:parenting_exception, m, context_stack.map {|a| a.class }, current_context.map {|a| a.class }, parent.inspect] 
          super
        end
        
        # if parent.respond_to?(m)
        #   parent.send(m,*args,&block)
        # else
        #   p [:parent, parent.class, m]
        #   super
        # end
      end
    end
  end
  def self.included(receiver)
    receiver.extend         ClassMethods
    receiver.send :include, InstanceMethods
  end
end