module PoolParty
  module Installers
    class BaseInstaller

      include ::PoolParty::Remote
      include ::PoolParty::Pinger
      
      def initialize
        self.__send__ :welcome_message
        commands.each {|c| self.__send__ c.to_sym }
        self.__send__ :closing_message
      end
      
      def welcome_message
        welcome_msg = <<-EOE
We are going to take you through the installation process of PoolParty.

First, we'll setup your environment so using PoolParty will be a breeze
        EOE
                
        colored_say "Welcome to PoolParty!", :help
        say welcome_msg
        begin
          t = colored_ask "Press enter to continue or Ctrl+C to exit"
        rescue Exception => e
          say <<-EOE

Cancelled PoolParty installation

You can always restart this by typing:
  install-poolparty
EOE
          exit 0
        end
        
      end
      
      def closing_message
closing_message = <<-EOE
You are now set to ride the waves with PoolParty! You'll notice there is a clouds.rb file in your current directory. You can start your new cloud
by typing:

cloud start

You can start your clouds.rb. More samples are available here: 
  http://github.com/auser/poolparty-examples/tree/master
EOE
        say closing_message
      end
      
      def commands
        @commands ||= []
      end
      
      protected
      
      def ask_with_help(opts={}, &block)
        help_str = opts[:help]
        message = opts[:message]

        o = ask("#{message} (h for help)") do |q|    
          q.validate = opts[:validate] if opts.has_key?(:validate)
        end

        if %w(h H).include?(o)
          colored_say help_str, :help
          ask_with_help(opts, &block)
        else
          block.call(o)
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

      def colored_ask(str, color = :notice)
        setup_colors
        ask("<%= color(\"#{str}\", :#{color}) %>")
      end

      def colored_say(str, color = :headline)
        setup_colors
        say("<%= color(\"#{str}\", :#{color}) %>") 
      end

      def setup_colors
          unless @setup_colors
          ft = HighLine::ColorScheme.new do |cs|
            cs[:headline]         = [ :bold, :yellow, :on_black ]
            cs[:horizontal_line]  = [ :bold, :white, :on_blue]
            cs[:critical]         = [ :yellow, :on_red  ]
            cs[:error]            = [ :bold, :red  ]
            cs[:help]             = [ :bold, :white, :on_blue]
            cs[:notice]           = [ :blue, :on_white]
          end

          HighLine.color_scheme = ft
          @setup_colors = true
        end
      end
      

    end
  end
end

Dir["#{::File.dirname(__FILE__)}/*.rb"].each {|lib| require lib }