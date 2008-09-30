$:.unshift(File.dirname(__FILE__) + '/../lib')
require 'poolparty'

%w(test/spec).each do |library|
  begin
    require library
  rescue
    STDERR.puts "== Cannot run test without #{library}"
  end
end

# Dir["#{File.dirname(__FILE__)}/helpers/**"].each {|a| require a}

include PoolParty
extend PoolParty

Base.environment = "test"
Base.verbose = false

def stub_option_load
    @str=<<-EOS
:access_key:    
  3.14159
:secret_access_key:
  "pi"
    EOS
    @sio = StringIO.new
    StringIO.stub!(:new).and_return @sio
    Base.stub!(:open).with("http://169.254.169.254/latest/user-data").and_return @sio
    @sio.stub!(:read).and_return @str
    Base.reset!
end

def hide_output
  begin
    old_stdout = STDOUT.dup
    STDOUT.reopen(File.open((PLATFORM =~ /mswin/ ? "NUL" : "/dev/null"), 'w'))
    yield if block_given?
  ensure
    STDOUT.flush
    STDOUT.reopen(old_stdout)
  end
end

def wait_launch(time=5)
  pid = fork {yield}
  wait time
  Process.kill("INT", pid)
  Process.wait(pid, 0)
end

def read_file(path)
  require "open-uri"
  open(path).read
end

def stub_list_from_local_for(o)
  @list =<<-EOS
  master 192.168.0.1
  node1 192.168.0.2
  EOS
  @file = "filename"
  @file.stub!(:read).and_return @list
  o.stub!(:get_working_listing_file).and_return @file
  o.stub!(:open).and_return @file
  
  @ris = @list.split(/\n/).map {|line| PoolParty::Remote::RemoteInstance.new(line) }
end
def stub_list_from_remote_for(o)
  @sample_instances_list = [{:ip => "192.168.0.1", :name => "master"}, {:ip => "192.168.0.2", :name => "node1"}]
  @ris = @sample_instances_list.map {|h| PoolParty::Remote::RemoteInstance.new(h) }
  o.stub!(:list_from_remote).and_return @ris
  o.stub!(:instances_list).once.and_return @ris
end