module CloudProviders
  class RdsInstance < Ec2Helper
    property :username, :password, :storage, :instance_class, :engine

    def authorize(options)
      options.each do |key, value|
        authorizations[key.to_s] = value
      end
    end

    def database(*db_names)
      @databases ||= []
      @databases = @databases + db_names unless db_names.empty?
      @databases
    end
    alias_method :databases, :database

    def create!
      if should_create_rds_instance?
        puts "-----> Creating RDS Instance: #{instance_id}"
        create_rds_instance!
      end
    end

    def run
      create! # Just for now, while we migrate to 2 commands
      authorize_access
      # TODO : wait until accessible?
    end

    def teardown
      puts "-----> Tearing down RDS Instance: #{instance_id}"
      delete_rds_instance!
    end

    def current_status
      rds_instances.detect{|i| i.DBInstanceIdentifier == instance_id }
    end

    def exists?
      ! current_status.nil?
    end

    def available?
      exists? && current_status.DBInstanceStatus == 'available'
    end

    def instance_id
      name.to_s
    end

  private

    def authorizations
      @authorizations ||= {}
    end

    def after_initialized
      raise "username must be specified" if self.username.nil?
      raise "password must be specified" if self.password.nil?
      raise "invalid password format (letters and digits only)" unless self.password =~ /[a-z][a-z0-9]*/i
      raise "EC2 user id must be defined in ENV or config" if Ec2.default_user_id.nil?
    end

    def rds_instances
      @rds_instances ||= (rds.describe_db_instances.DescribeDBInstancesResult.DBInstances || {})['DBInstance'] || []
      @rds_instances = [@rds_instances] unless @rds_instances.is_a?(Array)
      @rds_instances
    end

    def should_create_rds_instance?
      ! exists?
    end

    def create_rds_instance!
      db_name = (databases.shift || instance_id).to_s.gsub(/\-/, '_')
      params = {
        :db_instance_identifier => instance_id,
        :allocated_storage      => storage || 5,
        :db_instance_class      => instance_class || "db.m1.small",
        :engine                 => engine || "MySQL5.1",
        :master_username        => username,
        :master_user_password   => password,
        :db_name                => db_name
      }

      # TODO : optional params : :port, :db_parameter_group, :db_security_groups, :availability_zone, :preferred_backup_window, :backend_retention_period
      rds.create_db_instance(params)

      # TODO : create additional databases
    end

    def delete_rds_instance!
      rds.delete_db_instance(:db_instance_identifier => instance_id, :skip_final_snapshot => "true")
    end

    def authorize_access
      authorizations[:security_groups] = cloud.security_groups.map{|sg| sg.name } if authorizations[:security_groups].nil?
      authorizations.each do |type, values|
        [*values].each do |value|
          begin
            # TODO : allow customization of db sec group name
            params = {:db_security_group_name => "default"}

            if type.to_s =~ /network/
              puts "authorizing NET access for #{value}..."
              params[:cidrip] = value
            else
              puts "authorizing SECGRP access for #{value}/#{Ec2.default_user_id}..."
              params[:ec2_security_group_name] = value
              params[:ec2_security_group_owner_id] = Ec2.default_user_id
            end

            rds.authorize_db_security_group(params)
          rescue AWS::Error => e
            raise e unless e.message =~ /Authorization already exists/
          end
        end
      end
    end
  end
end
