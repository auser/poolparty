require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/../../../lib/poolparty/helpers/hash_printer'

describe "Hash printer" do
  before :all do
    @cloud_reference_hash = {
      :options => {:name => "dog", :keypair => "bob", :users => ["ari", "michael"]},
      :resources => {
        :file =>  [
                    {:name => "/etc/motd", :content => "Welcome to the cloud"},
                    {:name => "/etc/profile", :content => "profile info"}
                  ],
        :directory => [
                        {:name => "/var/www"}
                      ]    
      },
      :services => {
        :apache => {
          :options => {:listen => "8080"},
          :resources => {
                          :file => [
                              {:name => "/etc/apache2/apache2.conf", :template => "/absolute/path/to/template", :content => "rendered template string"}
                            ]
                        },
          :services => {}
        }
      }
    }
  end
  it "should print_to_string output a string" do
    # puts HashPrinter.print_to_string(@cloud_reference_hash)
    HashPrinter.print_to_string(@cloud_reference_hash).class.should == String
  end
end