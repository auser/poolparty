require "#{File.dirname(__FILE__)}/../../../test_helper"
require File.dirname(__FILE__)+"/ec2_test.rb"

class Ec2ResponseTest < Test::Unit::TestCase
  include CloudProviders
  def setup
    @provider = CloudProviders::Ec2.new(:image_id => "ami-abc123")
    @response =  @provider.ec2.describe_instances
    @described = Ec2Response.describe_instances(@response)
  end
  
  # def test_responds_to_core_methods
  #   %w(describe_instance 
  #      describe_instances
  #      pp_format
  #      convert_from_ec2_dns_to_ip
  #      parse_datetime).each do |meth|
  #        assert_respond_to CloudProviders::Ec2Response, meth
  #      end
  # end
  
  def test_returns_pp_format
    inst = Ec2Response.pp_format(@response.first)
    assert_not_nil inst[:public_ip]
    
  end
  
  def test_describe_instance
    assert_kind_of Array, @described
    assert_equal 2, @described.size
    assert @described.first.public_ip
    assert 'sdf', @described.first.keypair_name
  end
  
  def test_convert_from_ec2_dns_to_ip
    parsed = Ec2Response.convert_from_ec2_dns_to_ip("ip-10-250-46-144.ec2.internal")
    assert_equal '10.250.46.144', parsed
    assert_equal '75.101.141.103', Ec2Response.convert_from_ec2_dns_to_ip("ec2-75-101-141-103.compute-1.amazonaws.com")
    
    assert_equal '10.250.46.144', @described.first.internal_ip
    assert_equal '75.101.141.103', @described.first.public_ip
    
    assert_equal '10.0.40.5', Ec2Response.convert_from_ec2_dns_to_ip("10.0.40.5")
    assert_nil Ec2Response.convert_from_ec2_dns_to_ip()
  end
  
  def test_date_time_parsing
    assert_kind_of Time, Ec2Response.parse_datetime('2009-07-20T20:35:51.000Z')
    assert_equal 2009, @described.first.launch_time.year
  end
  
  
end
