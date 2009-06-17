# TODO: UPDATE THIS
# require File.dirname(__FILE__) + '/../spec_helper'
# 
# describe "Sshkey" do
#   describe "instances" do
#     before(:each) do
#       @key = Key.new
#       Key.stub!(:new).and_return @key
#       @key.stub!(:content).and_return "DIGITSOFTHEKEY"
#       @tc = TestBaseClass.new do
#         has_sshkey({:name => "~/.ssh/id_rsa"})
#       end
#       @dir = @tc.resource(:sshkey).first
#     end
#     it "have the name in the options" do
#       @dir.name.should == "~/.ssh/id_rsa"
#     end
#     it "should store the owner's name" do
#       @dir.enctype.should == "rsa"
#     end
#     describe "into PuppetResolver" do
#       before(:each) do
#         @compiled = PuppetResolver.new(@tc.to_properties_hash).compile
#       end
#       it "should set the filename to the name of the file" do
#         @compiled.should match(/sshkey \{ "~\/\.ssh\/id_rsa"/)
#       end
#       it "should say it's a sshkey in the ensure method" do
#         @compiled.should match(/ensure => "present"/)
#       end
#       it "have the mode set in the puppet output" do
#         @compiled.should match(/type => "rsa"/)
#       end
#       it "have the long string of digits key" do
#         @compiled.should match(/key => "DIGITSOFTHEKEY"/)
#       end
#     end
#   end
# end
