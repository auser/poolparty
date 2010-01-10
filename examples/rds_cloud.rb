# example pool with RDS enabled

pool :poolparty do
  cloud :app do
    using :ec2

    # this block will create an RDS DB instance
    #   by default, the instance id will match the containing cloud
    #   ("poolparty-app" in this example)
    rds do
      username "admin"                 # required
      password "secret"                # required

      # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      # these properties have the following overridable defaults:
      #
      # storage 5
      # instance_class "db.m1.small"
      # engine "db.m1.small"



      # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      # by default, a user DB will be created with a name that matches the containing cloud...
      # ...except RDS does not allow hyphens in DB names, so it will be pool name *underscore* cloud name
      # ("poolparty_app" in this example).
      #
      # to override this, use the database method, like so:
      # database :db1
      #   - or -
      # databases :production, :staging   # :databases is aliased to :database for nice DSL reading.  :)



      # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      # You are required to explicitly open access to the DB server to IP addresses (for outside EC2) or security groups (for inside EC2)
      # access will be granted to your containing cloud's security groups unless you explicitly list security group authorization.
      #
      # to override, use the :authorize method, like so:
      # authorize :networks => "1.2.3.4/24"  # <--- will authorize access to this CIDR block *in addition to* your EC2 security group
      # authorize :networks => ["1.2.3.4/32", "10.10.10.10/24"], :security_groups => "my_ec2_group"
    end
  end
end