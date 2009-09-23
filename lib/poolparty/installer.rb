$LOAD_PATH.unshift(File.dirname(__FILE__))
$LOAD_PATH.unshift(File.dirname(__FILE__) + "/poolparty")

require "poolparty"

module PoolParty
  class Installer

    include PoolParty::Pinger
    include Askable
        
    def run
      begin
        self.__send__ :welcome_message
        steps.each {|c| self.__send__ c.to_sym }
        self.__send__ :closing_message
      rescue SystemExit => e
      rescue Exception => e
        colored_say exit_msg
      ensure
        Colors.reset!
      end
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
<blue>cloud setup</blue>

<line>
EOE


      colored_say "<yellow>Welcome to PoolParty!</yellow>"
      colored_say welcome_msg
      colored_ask "Press enter to continue or Ctrl+C to exit", :no_value => true
      
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
    
    def exit_msg
      @exit_msg || <<-EOE
<red>--- quiting ---</red>
You can always restart the installer by typing #{$0}. 

If you need help, feel free to stop by the irc room:
irc.freenode.net / #poolpartyrb
      EOE
    end
    
    
    def steps
      @steps ||= []
    end
    
    def self.name
    end
    
    def self.description
    end
    
    def self.to_s
      "#{name} - #{description}"
    end
    
    def self.inherited(subclass)
      all << subclass
    end
    
    def self.all
      @all ||= []
    end
    
    def self.find_by_name(nm)
      all.detect do |i|
        i.name =~ /#{nm}/i
      end
    end
  end
end

%w(vmware ec2).each do |lib|
  require "installers/#{lib}"
end