# test RDS pool

pool :poolparty do
  cloud :fake_cloud do
    keypair File.dirname(__FILE__)+"/../keys/test_key"
    using :ec2

    rds :db1 do
    end
  end
end