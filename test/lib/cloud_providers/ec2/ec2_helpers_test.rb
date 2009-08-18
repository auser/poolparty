require "#{File.dirname(__FILE__)}/../../../test_helper"
require File.dirname(__FILE__)+"/ec2_test.rb"
# require 'rr'

class Ec2HelpersTest < Test::Unit::TestCase
  # include RR::Adapters::TestUnit
  include CloudProviders
  

  def inst
    @inst ||= clouds['app'].describe_instances.first
  end
  
  def test_associate_address
  end
  
  def test_next_unused_elastic_ip
  end
  
  def create_snapshot
  end
  
  def test_set_aws_env_from_yml_file
  end
  
  def test_save_aws_env_to_yml
  end
  
end