require "colors"
module Askable
  module ClassMethods
    def ask(msg="", opts={}, &block)
      begin
        answer = Question.new(msg, STDIN).answer
        
        unless opts[:no_value]
          if answer.nil? || answer == ""
            colored_say("You must enter a value\nTry again\n")
            ask(msg, &block)
          end
        end
      rescue Exception => e
        colored_say e.inspect
        exit 0
      end
    end

    def ask_with_help(opts={}, &block)
      help_str = opts[:help]
      message = opts[:message]

      colored_say("#{message} (h for help)")
      o = ask("? ")
      
      if %w(h H).include?(o)
        colored_say help_str
        ask_with_help(opts, &block)
      else
        block.call(o) if block
      end
      o
    end
    
    def colored_ask(str, o={})
      ask(substitute_color_tags(str), o)
    end
  
    def rescued_ask(m, r)
      begin
        t = colored_ask m
      rescue Exception => e
        say r
      end
    end
    
    def colored_say(str, o={})
      out = substitute_color_tags(str)
      o[:no_newline] ? print(out) : puts(out)
    end
    
    def colored_print(str)
      print substitute_color_tags(str)
    end
    
    def choose(str, choices={}, opts={}, &block)
      colored_say(str)
      choices.each do |k,v|
        colored_say("#{k}) #{v}")
      end
      colored_print((opts[:prompt] || "> "))
      pick = gets.chomp.to_i
      yield choices[pick] if block
      choices[pick]
    end
    
    def wait
      begin        
        STDIN.readline unless auto_install # -y passed
      rescue Interrupt
        exit 2
      end
  	end
  	
  	def substitute_color_tags(data)
      Colors.process(data)
    end
    
  end
  
  module InstanceMethods
    def ask(msg, &block)
      self.class.ask(msg, &block)
    end

    def ask_with_help(opts={}, &block); self.class.ask_with_help(opts, &block);end

    def colored_ask(str, o)
      self.class.colored_ask(str, o)
    end

    def colored_say(str, o={})
      self.class.colored_say(str, o)
    end

    def colored_print(str)
      self.class.colored_print(str)
    end
    
    def choose(str, choices={}, opts={}, &block); self.class.choose(str, choices, opts, &block); end
  end
  
  def self.included(receiver)
    receiver.extend         ClassMethods
    receiver.send :include, InstanceMethods
  end
  
  class Question
    attr_reader :answer
    include Askable
    
    def initialize(question, input=STDIN, opts={})
      @question = question
      @input = input
      
      defaults = {
        :wrap => 60
      }
      
      @opts = defaults.merge(opts)
      
      ask
    end
    
    private
    
    # get the answer, chomp it, etc.
    def ask
      case @question
      when String
        ask_string
      when Array
        ask_array
      end
    end
    
    def ask_string
      colored_say @question, :no_newline => true
      @answer = @input.gets
      answer.chomp!
    end
    
    def ask_array
      @question.each_with_index do |q, i|
        colored_say "#{i+1}> #{q}"
      end
      colored_say "(number 1-#{@question.size})"
      colored_say "? ", :no_newline => true
      num = (@input.gets).to_i rescue colored_say("<red>Invalid input, must be a number between 1 and #{@question.size + 1}")
      @answer = @question[num-1]
    end
  end
end