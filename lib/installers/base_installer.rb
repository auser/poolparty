module PoolParty
  module Installers
    class Base

      include ::PoolParty::Pinger
      
      def go
        begin
          self.__send__ :welcome_message
          steps.each {|c| self.__send__ c.to_sym }
          self.__send__ :closing_message
        ensure
          reset_terminal_colors!
        end
      end
      
      def self.name
      end
      
      def self.description
      end
      
      def welcome_message
        welcome_msg = <<-EOE
We are going to take you through the installation process of PoolParty.

First, we'll setup your environment so using PoolParty will be a <blue>breeze</blue>
        EOE

          @exit_msg = <<-EOE

<line>
<yellow>Cancelled PoolParty installation</yellow>

You can always restart this by typing:
  <blue>install-poolparty</blue>

<line>
EOE


        colored_say "<yellow>Welcome to PoolParty!</yellow>"
        colored_say welcome_msg
        colored_ask "Press enter to continue or Ctrl+C to exit"
        
      end
      
      def closing_message
closing_message = <<-EOE
<line>
You are now set to <blue>ride the waves</blue> with PoolParty! You'll notice there is a <blue>clouds.rb</blue> file in your current directory. 

You can start your new cloud by typing in this directory:

  <blue>cloud start [-v]</blue>

You can start your clouds.rb. More samples are available here: 
  <yellow>http://github.com/auser/poolparty-examples/tree/master</yellow>
  
<line>
EOE
        colored_say closing_message
      end
      
      def steps
        @steps ||= []
      end
      
      protected
      
      def ask(msg="", &block)
        begin
          @answer = Question.new(msg, STDIN).answer
        rescue Exception => e                    
          colored_say exit_msg
          exit 0
        end
      end
      
      def exit_msg
        @exit_msg || <<-EOE
<red>--- quiting ---</red>
You can always restart the installer by typing #{$0}. 

If you need help, feel free to stop by the irc room:
irc.freenode.net / #poolpartyrb
        EOE
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

      def rescued_ask(m, r)
        begin
          t = colored_ask m
        rescue Exception => e
          say r
        end
      end

      def colored_ask(str)
        ask(substitute_color_tags(str))
      end

      def colored_say(str, o={})
        out = substitute_color_tags(str)
        o[:no_newline] ? print(out) : puts(out)        
      end
      
      def colored_print(str)
        print substitute_color_tags(str)
      end
            
      def wait
        begin        
          STDIN.readline unless auto_install # -y passed
        rescue Interrupt
          exit 2
        end
    	end    	
      
      # COLORED OUTPUT
      # Taken from the wonderful guys at Ruby Enterprise
      def default_terminal_colors
        @default_terminal_colors ||= "\e[0m\e[37m\e[40m"
      end
      
      def substitute_color_tags(data)	
        data.gsub!(/<line>/, "--------------------------------------------------------------------")
        
        # Backrounds
        colors = {:red => 1, :green => 2, :yellow => 3, :blue => 4, :purple => 5, :sea => 6, :white => 7}
        
        # backgrounds
        if m = data.match(%r{<(.*?) bg=(.*?)>(.*?)<\/(.*?)>}m)          
          colors.each do |k,v|
            t = data.match(%r{<(.*?) bg=#{k}>(.*?)<\/(.*?)>}m)
            data.gsub!(%r{<(.*?) bg=#{k}>(.*?)<\/(.*?)>}m, "\e[1m\e[4#{v}m<\\1>\\2</\\1>#{default_terminal_colors}")
          end
        end

        # Colored text
      	colors.each do |k,v|
          data.gsub!(%r{<#{k}>(.*?)</#{k}>}m, "\e[1m\e[3#{v}m\\1#{default_terminal_colors}")
      	end
        
        # Special characters        
      	data.gsub!(%r{<b>(.*?)</b>}m, "\e[1m\\1#{default_terminal_colors}")
      	data.gsub!(%r{<banner>(.*?)</banner>}m, "\e[33m\e[44m\e[1m\\1#{default_terminal_colors}")
      	return data
      end
    	
      def reset_terminal_colors!
        STDOUT.write("\e[0m")
    		STDOUT.flush
      end
      

    end
    class Question < Base
      attr_reader :answer
      
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
end

Dir["#{::File.dirname(__FILE__)}/*.rb"].each {|lib| require lib }