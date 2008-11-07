require File.dirname(__FILE__) + '/../spec_helper'

describe "Base" do
  before(:each) do
    ENV.stub!(:[]).with("AWS_ACCESS_KEY_ID").and_return "KEY"
    ENV.stub!(:[]).with("AWS_SECRET_ACCESS_KEY").and_return "SECRET"
  end
  it "should set the environment, if not set to production" do
    Base.environment.should == "production"
  end
  it "should set the user to root" do
    Base.user.should == "root"
  end
  it "should set the base keypair path to ~/.ec2" do
    Base.base_keypair_path.should == "~/.ec2"
  end
  it "should set the storage_directory to the tmp directory of the current working directory" do
    Base.storage_directory.should =~ /tmp/
  end
  it "should set the tmp path to tmp" do
    Base.tmp_path.should == "/tmp/poolparty"
  end
  it "should set the remote storage path to /var/poolparty" do
    Base.remote_storage_path.should == "/var/poolparty"
  end
  it "should set the fileserver_base to puppet://" do
    Base.fileserver_base.should =~ /puppet:\/\//
  end
  it "should have an access key" do
    Base.access_key.should == "KEY"
  end
  it "should have a secret access key" do
    Base.secret_access_key.should == "SECRET"
  end
  describe "keys" do
    it "should have an array of key_file_locations" do
      Base.key_file_locations.class.should == Array
    end
    it "should test if the files exist when looking for the file" do
      ::File.stub!(:file?).and_return false
      ::File.stub!(:file?).with("ppkeys").and_return true
      Base.get_working_key_file_locations.should == "ppkeys"
    end
    it "should call get_working_key_file_locations" do
      @str = "foo"
      @str.stub!(:read).and_return true
      Base.stub!(:open).and_return @str
      Base.should_receive(:get_working_key_file_locations)
      Base.read_keyfile
    end
    describe "with keyfile" do
      before(:each) do
        @keyfile = "ppkeys"
        @str = "---
        :access_key: KEY
        :secret_access_key: SECRET"
        @keyfile.stub!(:read).and_return @str
        Base.stub!(:get_working_key_file_locations).and_return @keyfile
        Base.stub!(:read_keyfile).and_return @str
        Base.stub!(:open).and_return @str
        Base.reset!
      end
      it "should call YAML::load on the working key file" do
        YAML.should_receive(:load).with(@str)
        Base.load_keys_from_file
      end
      it "should return a hash" do
        Base.load_keys_from_file.class.should == Hash
      end
      it "should be able to fetch the access key from the loaded keys" do
        Base.load_keys_from_file[:access_key].should == "KEY"
      end
      it "should be able to fetch the secret_access_key from the loaded key file" do
        Base.load_keys_from_file[:secret_access_key].should == "SECRET"
      end
      describe "without keyfile" do
        before(:each) do
          Base.stub!(:get_working_key_file_locations).and_return nil
          ENV.stub!(:[]).with("AWS_ACCESS_KEY_ID").and_return nil
          ENV.stub!(:[]).with("AWS_SECRET_ACCESS_KEY").and_return nil
          Base.reset!
        end
        it "should render the access_key nil" do
          Base.access_key.should == nil
        end
        it "should render the secret_access_key as nil" do
          Base.secret_access_key.should == nil
        end
      end
    end
    describe "storing keyfile" do
      before(:each) do
        @ak = "KEY"
        @pk = "SECRET"
        @str = "weee"
        @hash = {:access_key => @ak, :secret_access_key => @pk}
        Base.stub!(:access_key).and_return @ak
        Base.stub!(:secret_access_key).and_return @pk
        Base.stub!(:write_to_file).and_return true
        Base.stub!(:key_file_locations).and_return ["ppkey"]
      end
      it "should call access_key.nil?" do
        @ak.should_receive(:nil?).once
      end
      it "should call YAML::dump" do
        YAML.should_receive(:dump).and_return @str
      end
      it "should call write_to_file with the key file location" do
        Base.should_receive(:write_to_file).with("ppkey", YAML::dump(@hash)).and_return true
      end
      after(:each) do
        Base.store_keys_in_file
      end
    end
    describe "allowed_commands" do
      before(:each) do
        @str =<<-EOE
echo 'hello world'
        EOE
        @str.stub!(:read).and_return @str
        Base.stub!(:open).and_return @str
      end
      it "should load the yaml file allowed_commands.yml" do
        Base.allowed_commands.class.should == Array
      end
      it "should have the command echo 'hello world' in the list of allowed_commands" do
        Base.allowed_commands.include?("echo 'hello world'").should == true
      end
      it "should not have the command rm -rf / in the list of allowed commands" do
        Base.allowed_commands.include?("rm -rf /").should == false
      end
    end
  end
end