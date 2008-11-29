require File.dirname(__FILE__) + '/../../spec_helper'
require File.dirname(__FILE__) + '/../../../../lib/poolparty/spec/core/string'

describe "Spec strings" do
  before(:each) do
    @str=<<-EOE
exec { 		"install_passenger_script":
				unless => 'test -f /etc/apache2/conf.d/passenger.conf',
				path => '/usr/bin:/bin:/usr/local/bin:$PATH',
				command => '/usr/bin/ruby /var/poolparty/install_passenger.rb'
		}
		service { 		"apache2":
    				require => Package['apache2'],
    				name => 'apache2',
    				ensure => 'running'
    		}
    
exec { 		"force-reload-apache2":
				path => '/usr/bin:/bin:/usr/local/bin:$PATH',				
				command => '/etc/init.d/apache2 force-reload',
				refreshonly => 'true'
		}    
    EOE
  end
  it "should have the method grab_entry_for" do
    "hi".respond_to?(:grab_entry_for).should == true
  end
  describe "grab_entry_for" do
    it "should be able to grab the entry for the apache2 service" do
      @str.grab_entry_for(:service, "apache2").gsub(/[\n\t ]+/, '').should == "service{\"apache2\":require=>Package['apache2'],name=>'apache2',ensure=>'running'}"
    end
    it "should be able to grab the entry for the install_passenger_script exec" do
      @str.grab_entry_for(:exec, "install_passenger_script").gsub(/[\n\t ]+/, '').should == "exec{\"install_passenger_script\":unless=>'test-f/etc/apache2/conf.d/passenger.conf',path=>'/usr/bin:/bin:/usr/local/bin:$PATH',command=>'/usr/bin/ruby/var/poolparty/install_passenger.rb'}"
    end
    it "should return an empty string if there is no entry" do
      @str.grab_entry_for(:pocket, "rangers").should == ""
    end
  end
  describe "grab_key_value_for" do
    it "should be able to say the service apache2 is set to ensure running" do
      @str.grab_key_value_for(:service, "apache2", :ensure).should == "'running'"
    end
    it "should be able to say the exec force-reload-apache2 has the command '/etc/init.d/apache2 force-reload'" do
      @str.grab_key_value_for(:exec, "force-reload-apache2", :command).should == "'/etc/init.d/apache2 force-reload'"
    end
    it "should be able to grab them with strings with quotes" do
      "exec { 		\"a2enmod ssl\":
      				command => 'a2enmod ssl',
      				require => [ Package['apache2-common'], Package['openssl'] ],
      				path => '/usr/bin:/bin:/usr/local/bin:$PATH',
      				unless => '/usr/bin/test -L /etc/apache2/mods-enabled/ssl.load',
      				notify => Exec['restart-apache2']
      		}
      ".grab_key_value_for(:exec, "a2enmod ssl", :notify).should == "Exec['restart-apache2']"
    end
  end
end