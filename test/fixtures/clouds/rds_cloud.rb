# test RDS pool

pool :poolparty do
  cloud :fake_cloud do
    keypair File.dirname(__FILE__)+"/../keys/test_key"
    using :ec2

    rds :db1 do
      username "admin"
      password "secret"
      storage 5
    end
  end
end