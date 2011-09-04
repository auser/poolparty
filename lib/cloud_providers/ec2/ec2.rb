=begin rdoc
  EC2 CloudProvider
  This serves as the basis for running PoolParty on Amazon's ec2 cloud.
=end

module CloudProviders
  class Ec2 < CloudProvider
    # Set the aws keys from the environment, or load from /etc/poolparty/env.yml if the environment variable is not set
    def self.default_access_key
      ENV['EC2_ACCESS_KEY'] || load_keys_from_file[:access_key] || load_keys_from_credential_file[:access_key]
    end

    def self.default_secret_access_key
      ENV['EC2_SECRET_KEY'] || load_keys_from_file[:secret_access_key] || load_keys_from_credential_file[:secret_access_key]
    end

    def self.default_private_key
      ENV['EC2_PRIVATE_KEY'] || load_keys_from_file[:private_key]
    end

    def self.default_cert
      ENV['EC2_CERT'] || load_keys_from_file[:cert]
    end

    def self.default_user_id
      ENV['EC2_USER_ID'] || load_keys_from_file[:user_id]
    end

    def self.default_ec2_url
      ENV['EC2_URL'] || load_keys_from_file[:ec2_url]
    end

    def self.default_s3_url
      ENV['S3_URL'] || load_keys_from_file[:s3_url]
    end

    def self.default_cloud_cert
      ENV['CLOUD_CERT'] || ENV['EUCALYPTUS_CERT'] || load_keys_from_file[:cloud_cert]
    end

    def self.default_credential_file
      ENV['AWS_CREDENTIAL_FILE'] || load_keys_from_file[:credential_file]
    end

    # Load the yaml file containing keys.  If the file does not exist, return an empty hash
    def self.load_keys_from_file(filename=POOLPARTY_CONFIG_FILE, caching=true)
      return @aws_yml if @aws_yml && caching==true
      return {} unless File.exists?(filename)
      puts("Reading keys from file: #{filename}")
      @aws_yml = YAML::load( open(filename).read ) || {}
    end

    # Load credentials from file
    def self.load_keys_from_credential_file(filename=default_credential_file, caching=true)
      return {:access_key => @access_key, :secret_access_key => @secret_access_key} if (@access_key && @secret_access_key && caching)
      return {} if filename.nil? or not File.exists?(filename)
      puts("Reading keys from file: #{filename}")
      File.open(filename).each_line do |line|
        if line =~ /AWSAccessKeyId=([a-zA-Z0-9]+)$/
          @access_key=$1.chomp
        elsif line =~ /AWSSecretKey=([^   ]+)$/
          @secret_access_key=$1.chomp
        end
      end
      return {:access_key => @access_key, :secret_access_key => @secret_access_key}
    end

    default_options(
      :instance_type          => 'm1.small',
      :availability_zones     => ["us-east-1a"],
      :user_id                => default_user_id,
      :private_key            => default_private_key,
      :cert                   => default_cert,
      :cloud_cert             => default_cloud_cert,
      :access_key             => default_access_key,
      :secret_access_key      => default_secret_access_key,
      :ec2_url                => default_ec2_url,
      :s3_url                 => default_s3_url,
      :credential_file        => default_credential_file,
      :min_count              => 1,
      :max_count              => 1,
      :user_data              => '',
      :kernel_id              => nil,
      :ramdisk_id             => nil,
      :block_device_mapping   => [{}],
      :subnet_id              => nil,
      :spot_price             => nil,
      :launch_group           => nil,
      :spot_persistence       => nil,
      :disable_api_termination => nil,
      :instance_initiated_shutdown_behavior => nil
    )

    # Called when the create command is called on the cloud
    def create!
      [:security_groups, :load_balancers, :rds_instances].each do |type|
        self.send(type).each {|ele| ele.create! }
      end
    end

    def run
      puts "  for cloud:         #{cloud.name}"
      puts "  minimum_instances: #{minimum_instances}"
      puts "  maximum_instances: #{maximum_instances}"
      puts "  security_groups:   #{security_group_names.join(", ")}"
      puts "  image id:          #{image_id}"
      puts "  using keypair:     #{keypair}"
      puts "  with user_data:    #{user_data.to_s.inspect[0..100]} ..."
      puts "  user:              #{user}"
      puts "  at spot price:     #{spot_price} #{spot_persistence}\n" if spot_price

      security_groups.each do |sg|
        sg.run
      end

      unless rds_instances.empty?
        rds_instances.each do |rdsi|
          puts "    rds instance: #{rdsi.name}"
          rdsi.run
        end
      end

      if autoscalers.empty? # not using autoscaling
        puts "---- live, running instances (#{nodes.size}) ----"
        if nodes.size < minimum_instances
          expansion_count = minimum_instances - nodes.size
          puts "-----> expanding the cloud because the #{expansion_count} minimum_instances is not satisified: "
          maybe('expand cloud') do
            expand_by(expansion_count)
          end
        elsif nodes.size > maximum_instances
          contraction_count = nodes.size - maximum_instances
          puts "-----> contracting the cloud because the instances count exceeds the #{maximum_instances} maximum_instances by #{contraction_count}"
          maybe('contract cloud') do
            contract_by(contraction_count)
          end
        end
        progress_bar_until("Waiting for the instances to be launched") do
          reset!
          running_nodes = nodes.select {|n| n.running? }
          running_nodes.size >= minimum_instances
        end
        reset!
        # ELASTIC IPS
      else
        autoscalers.each do |a|
          puts "    autoscaler: #{a.name}"
          puts "-----> The autoscaling groups will launch the instances"
          a.run

          progress_bar_until("Waiting for autoscaler to launch instances") do
            reset!
            running_nodes = nodes.select {|n| n.running? }
            running_nodes.size >= minimum_instances
          end
          reset!
        end
      end

      from_ports = security_groups.map {|a| a.authorizes.map {|t| t.from_port.to_i }.flatten }.flatten
      if from_ports.include?(22)
        progress_bar_until("Waiting for the instances to be accessible by ssh") do
          running_nodes = nodes.select {|n| n.running? }
          accessible_count = running_nodes.map do |node|
            node.accessible?
          end.size
          accessible_count == running_nodes.size
        end
      end

      unless load_balancers.empty?
        load_balancers.each do |lb|
          puts "    load balancer: #{lb.name}"
          lb.run
        end
      end

      assign_elastic_ips
      cleanup_ssh_known_hosts!
      puts "Attaching EBS volumes"
      assign_ebs_volumes # Assign EBS volumes
    end

    def teardown
      puts "------ Tearing down and cleaning up #{cloud.name} cloud"
      unless autoscalers.empty?
        puts "Tearing down autoscalers"
      end
    end

    def expand_by(num=1)
      e = Ec2Instance.run!({
          :image_id             => image_id,
          :min_count            => num,
          :max_count            => num,
          :key_name             => keypair.basename,
          :security_groups      => security_groups,
          :user_data            => decoded_user_data,
          :instance_type        => instance_type,
          :availability_zone    => availability_zones.first,
          :base64_encoded       => true,
          :cloud                => cloud,
          :block_device_mapping => block_device_mapping,
          :subnet_id            => subnet_id,
          :spot_price           => spot_price,
          :launch_group         => launch_group,
          :spot_persistence     => spot_persistence,
          :disable_api_termination => disable_api_termination,
          :instance_initiated_shutdown_behavior => instance_initiated_shutdown_behavior
        })
      return if e == 'spot instances requested'
      progress_bar_until("Waiting for node to launch...") do
        wait_for_node(e)
      end
      all_nodes.detect {|n| n.instance_id == e.instance_id }
    end

    def decoded_user_data
      if user_data
        if File.file?(user_data)
          open(user_data).read
        else
          user_data
        end
      end
    end

    def wait_for_node(instance)
      reset!
      inst = all_nodes.detect {|n| n.instance_id == instance.instance_id }
      inst.running? if inst
    end

    def contract_by(num=1)
      raise RuntimeError, "Contracting instances by #{num} will lower the number of instances below specified minimum" unless nodes.size - num > minimum_instances
      num.times do |i|
        node = nodes[-num]
        id = node.instance_id
        node.ssh_cleanup_known_hosts!
        Ec2Instance.terminate!(:instance_id => id, :cloud => cloud)
      end
      reset!
    end

    def bootstrap_nodes!(tmp_path=nil)
      unless security_groups.map {|a| a.authorizes.map {|t| t.from_port.to_i }.flatten }.flatten.include?(22)
        warn "Cloud security_groups are not authorized for ssh. Cannot bootstrap."
        return
      end
      tmp_path ||= cloud.tmp_path
      nodes.each do |node|
        next unless node.in_service?
        node.cloud_provider = self
        node.rsync_dir(tmp_path)
        node.bootstrap_chef!
        node.run_chef!
      end
    end

    def configure_nodes!(tmp_path=nil)
      # removed duplicated code (now configure_nodes! invokes
      # node.bootstrap_chef!, while old version did not, but I believe
      # this is harmless)
      bootstrap_nodes!(tmp_path)

      ebs_volume_groups.each do |vol_grp|
        vol_grp.verify_attachments nodes
      end
    end

    def assign_elastic_ips
      unless elastic_ips.empty?
        unused_elastic_ip_addresses = ElasticIp.unused_elastic_ips(self).map {|i| i.public_ip }
        used_elastic_ip_addresses = ElasticIp.elastic_ips(self).map {|i| i.public_ip }

        elastic_ip_objects = ElasticIp.unused_elastic_ips(self).select {|ip_obj| elastic_ips.include?(ip_obj.public_ip) }

        assignee_nodes = nodes.select {|n| !ElasticIp.elastic_ips(self).include?(n.public_ip) }

        elastic_ip_objects.each_with_index do |eip, idx|
          # Only get the nodes that do not have elastic ips associated with them
          begin
            if assignee_nodes[idx]
              puts "Assigning elastic ip: #{eip.public_ip} to node: #{assignee_nodes[idx].instance_id}"
              ec2.associate_address(:instance_id => assignee_nodes[idx].instance_id, :public_ip => eip.public_ip)
            end
          rescue Exception => e
            p [:error, e.inspect]
          end
          reset!
        end
      end
    end

    def cleanup_ssh_known_hosts!(nodes_to_cleanup=nodes,
                                 even_unavailable=false)
      puts "cleaning up .ssh/known_hosts"
      nodes_to_cleanup.find_all do |node|
        even_unavailable || node.ssh_available?
      end.each do |node|
        node.ssh_cleanup_known_hosts!
      end
    end

    def nodes
      all_nodes.select {|i| i.in_service? }#describe_instances.select {|i| i.in_service? && security_groups.include?(i.security_groups) }
    end

    # === Description
    #
    # Return all the security groups of the instance that are prefixed with #poolparty.
    #
    # These are special security groups used only for tagging
    #
    # === Parameters
    #   instance    -   An ec2 instance as returned from describe_instances
    def tags(instance)
      return [] if instance.groupSet.nil? || instance.groupSet.item.empty?
      instance.groupSet.item.collect{|g| g.groupId }.select {|s| s.start_with? "#poolparty"}
    end

    def all_nodes
      @nodes ||= describe_instances.select { |i|
        !(security_group_names & tags(i)).empty?
      }.sort {|a,b|
        DateTime.parse(a.launchTime) <=> DateTime.parse(b.launchTime)
      }
    end

    # Describe instances
    # Describe the instances that are available on this cloud
    # @params id (optional) if present, details about the instance
    # with the id given will be returned
    # if not given, details for all instances will be returned
    def describe_instances(id=nil)
      begin
        @describe_instances = ec2.describe_instances.reservationSet.item.map do |r|
          r.instancesSet.item.map do |i|
            inst_options = i.merge(r.merge(:cloud => cloud)).merge(cloud.cloud_provider.dsl_options)
            Ec2Instance.new(inst_options)
          end
        end.flatten
      rescue AWS::InvalidClientTokenId => e # AWS credentials invalid
        puts "Error contacting AWS: #{e}"
        raise e
      rescue Exception => e
        []
      end
    end

    # Extras!

    def block_device_mapping(o=[], given_name=cloud.proper_name )
      @mappings ||= o
    end

    def load_balancer(given_name=cloud.proper_name, o={}, &block)
      load_balancers << ElasticLoadBalancer.new(given_name, sub_opts.merge(o || {}), &block)
    end
    def autoscale(given_name=cloud.proper_name, o={}, &block)
      autoscalers << ElasticAutoScaler.new(given_name, sub_opts.merge(o || {}), &block)
    end
    def security_group(given_name=cloud.proper_name, o={}, &block)
      existing_security_group = security_groups.detect{|grp| grp.name == given_name }
      if existing_security_group
        existing_security_group.merge!(sub_opts.merge(o || {}), &block)
      else
        security_groups << SecurityGroup.new(given_name, sub_opts.merge(o || {}), &block)
      end
    end
    def elastic_ip(*ips)
      ips.each {|ip| elastic_ips << ip}
    end

    def rds(given_name=cloud.proper_name, o={}, &block)
      rds_instances << RdsInstance.new(given_name, sub_opts.merge(o || {}), &block)
    end

    def available_rds_instances
      rds_instances.select{|r| r.available? }
    end
    
    # prepare options for AWS gem
    def aws_options(opts={})
      uri=URI.parse(ec2_url)
      { :access_key_id    => access_key, 
        :secret_access_key=> secret_access_key, 
        :use_ssl          => (uri.scheme=='https'), 
        :path             => uri.path, 
        :host             => uri.host,
        :port             => uri.port
      }.merge(opts)
      
    end

    # Proxy to the raw Grempe amazon-aws @ec2 instance
    def ec2
      @ec2 ||= begin
       AWS::EC2::Base.new( aws_options )
      rescue AWS::ArgumentError => e # AWS credentials missing?
        puts "Error contacting AWS: #{e}"
        raise e
      rescue Exception => e
        puts "Generic error #{e.class}: #{e}"
      end
    end

    # Proxy to the raw Grempe amazon-aws autoscaling instance
    def as
      @as = AWS::Autoscaling::Base.new( aws_options )
    end

    # Proxy to the raw Grempe amazon-aws elastic_load_balancing instance
    def elb
      @elb ||= AWS::ELB::Base.new( aws_options )
    end

    def awsrds
      @awsrds ||= AWS::RDS::Base.new( aws_options )
    end

    def security_group_names
      security_groups.map {|a| a.to_s }
    end
    def security_groups
      @security_groups ||= []
    end
    def load_balancers
      @load_balancers ||= []
    end
    def autoscalers
      @autoscalers ||= []
    end
    def elastic_ips
      @elastic_ips ||= []
    end

    def ebs_volume_groups
      @ebs_volume_groups ||= []
    end

    # dsl method for EBS volumes. E.G.:
    #   ebs_volumes do
    #     volumes "vol-001248ff", "vol-01ff4b85" # use existing volumes, not mandatory
    #     device "/dev/sdf"
    #     snapshot_id "snap-602030dd"
    #     size 200
    #   end
    def ebs_volumes(name=nil, &block)
      ebs_volume_groups << ElasticBlockStoreGroup.new(sub_opts,&block) if block
    end

    def assign_ebs_volumes
      ebs_volume_groups.each{|ebs_volume_group| ebs_volume_group.attach(nodes)}
    end

    def rds_instances
      @rds_instances ||= []
    end

    # Clear the cache
    def reset!
      @nodes = @describe_instances = nil
    end

    # Get existing volumes on EC2. filters is a hash of filters, either single valued or multivalued (value is an array of possible values).
    # The function will return volumes matching *all* filters. A volume is a filter match if *any* one of the filter values equals the volume parameter value.
    def list_ec2_volumes(filters=nil)
      @volumes_on_ec2=ec2.describe_volumes.volumeSet.item unless @volumes_on_ec2
      (if filters.nil? # no filter to check, so return at once
        @volumes_on_ec2
      else
        @volumes_on_ec2.select{|vol| # select volumes for which no filter failed
          not filters.map {|filter_key, filter_val|
            filter_key=filter_key.to_s if filter_key.is_a?(Symbol) # filter_key may be given as a symbol
            raise ArgumentError, "Filter key #{filter_key} is invalid" unless vol.has_key?(filter_key)
            if filter_val.is_a?(Array) # Deal with multiple filter values
              filter_val.map{|val| val.is_a?(String) ? val : val.to_s}.member?(vol[filter_key]) # make sure fiter_val array values are Strings before checking for match
            else
              (filter_val.is_a?(String) ? filter_val : filter_val.to_s)==vol[filter_key] # make sure fiter_val is a String before comparing
            end
            }.member?(false) # Check if a filter failed, the 'not' statement at the beginning of the map block negates this so 'select' will choose only when no filter failed
          }.compact # remove nil results from volume set.
        end
        ).map{|vol| ElasticBlockStore.new(vol,:cloud => cloud)}
    end

    # Read credentials from credential_file if one exists
    def credential_file(file=nil)
      unless file.nil?
        dsl_options[:credential_file]=file
        dsl_options.merge!(Ec2.load_keys_from_credential_file(file))
      else
        fetch(:credential_file)
      end
    end

    private
    # Helper to get the options with self as parent
    def sub_opts
      dsl_options.merge(:parent => self, :cloud => cloud)
    end
    def generate_keypair(n=nil)
      puts "[EC2] generate_keypair is called with #{default_keypair_path/n}"
      begin
        hsh = ec2.create_keypair(:key_name => n)
        string = hsh.keyMaterial
        FileUtils.mkdir_p default_keypair_path unless File.directory?(default_keypair_path)
        puts "[EC2] Generated keypair #{default_keypair_path/n}"
        puts "[EC2] #{string}"
        File.open(default_keypair_path/n, "w") {|f| f << string }
        File.chmod 0600, default_keypair_path/n
      rescue Exception => e
        puts "[EC2] The keypair exists in EC2, but we cannot find the keypair locally: #{n} (#{e.inspect})"
      end
      keypair n
    end

  end
end

require "#{File.dirname(__FILE__)}/ec2_instance"
require "#{File.dirname(__FILE__)}/helpers/ec2_helper"
%w( security_group
    authorize
    elastic_auto_scaler
    elastic_block_device_mapping
    elastic_block_store
    elastic_block_store_group
    elastic_load_balancer
    elastic_ip
    rds_instance
    revoke).each do |lib|
  require "#{File.dirname(__FILE__)}/helpers/#{lib}"
end
