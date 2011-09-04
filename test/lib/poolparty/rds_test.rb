require 'test_helper'

class RdsTest < Test::Unit::TestCase
  def setup
    stub_ec2_calls
    stub_response(AWS::EC2::Base, :describe_security_groups, 'ec2-describe-security-groups')
    stub_response(AWS::EC2::Base, :run_instances,            'ec2-run-instances')
    stub_response(AWS::RDS::Base, :describe_db_instances,    'rds-describe-db-instances-empty')
    reset!
  end

  def test_basic
    scenario "rds_cloud"
  end

  def test_required_properties
    assert_raises(RuntimeError) { scenario "rds_missing_params" }
  end

  private

  def scenario(filename)
    clear!

    @filepath = File.join(FIXTURES_PATH, "clouds/#{filename}.rb")
    require @filepath
    @cloud = pool.clouds[pool.clouds.keys.first]

    @cloud.run
  end

  def stub_response(klass, method, fixture_filename)
    klass.any_instance.stubs(method).returns AWS::Response.parse(:xml => open(File.join(FIXTURES_PATH, "ec2/#{fixture_filename}_response_body.xml")).read)
  end
end