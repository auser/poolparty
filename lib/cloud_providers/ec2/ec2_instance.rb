module CloudProviders
  class Ec2Instance < RemoteInstance

    default_options(
      :security_groups      => [],
      :private_ip           => nil,
      :dns_name             => nil,
      :instance_type        => nil,
      :public_ip            => nil,
      :key_name             => nil,
      :launch_time          => nil,
      :availability_zones   => [],
      :block_device_mapping => [{}],
      :subnet_id            => nil,
      :spot_price           => nil,
      :launch_group         => nil,
      :spot_persistence     => nil,
      :disable_api_termination => nil,
      :instance_initiated_shutdown_behavior => nil
    )

    def initialize(raw_response={})
      @raw_response = raw_response
      self.instance_id                          = raw_response["instanceId"] rescue nil
      self.security_groups                      = raw_response.groupSet.item.map{|sg| sg.groupId }.sort rescue nil
      self.image_id                             = raw_response["imageId"] rescue nil
      self.private_ip                           = raw_response["privateIpAddress"] rescue nil
      self.dns_name                             = raw_response["dnsName"] rescue nil
      self.instance_type                        = raw_response["instanceType"] rescue nil
      self.public_ip                            = raw_response["ipAddress"] rescue nil
      self.key_name                             = raw_response["keyName"] rescue nil
      self.launch_time                          = raw_response["launchTime"] rescue nil
      self.availability_zones                   = raw_response["placement"]["availabilityZone"] rescue nil
      self.status                               = raw_response["instanceState"]["name"] rescue nil
      self.block_device_mapping                 = raw_response["blockDeviceMapping"] rescue nil
      self.subnet_id                            = raw_response["subnetId"] rescue nil
      self.launch_group                         = raw_response["launchGroup"] rescue nil
      # disable_api_termination and instance_initiated_shutdown_behavior don't currently get returned in the request -- you'd need to later call describe_instance_attribute
      self.disable_api_termination              = raw_response["disableApiTermination"] rescue nil
      self.instance_initiated_shutdown_behavior = raw_response["instanceInitiatedShutdownBehavior"] rescue nil
      super
    end

    def keypair(n=nil)
      return @keypair if @keypair
      @keypair = (cloud.keypair.basename == self.key_name) ? cloud.keypair : Keypair.new(self.key_name, cloud.keypair.extra_paths)
    end

    def security_group_names
      security_groups.map{|a| a.to_s }
    end

    def zone
      availability_zones.first
    end

    def reachable?
      ping_port self.public_ip, 22
    end

    def ssh_available?
      cloud.security_groups.map {|a|
        a.authorizes.map {|t| t.from_port.to_i }.flatten
      }.flatten.include?(22) and
        reachable? and
        in_service? and
        keypair and keypair.exists?
    end

    def in_service?
      running?
    end

    def run!
      if spot_price.to_f > 0
        request_spot_instances!
      else
        launch_instances!
      end
    end
    def self.run!(hsh); new(hsh).run!; end

    def launch_instances!
      r = cloud_provider.ec2.run_instances(
        :image_id             => image_id,
        :min_count            => min_count,
        :max_count            => max_count,
        :key_name             => keypair.basename,
        :security_group       => cloud.security_group_names,
        :user_data            => user_data,
        :instance_type        => instance_type,
        :availability_zone    => availability_zone,
        :block_device_mapping => block_device_mapping,
        :disable_api_termination => disable_api_termination,
        :instance_initiated_shutdown_behavior => instance_initiated_shutdown_behavior,
        :base64_encoded       => true)
      r.instancesSet.item.map do |i|
        inst_options = i.merge(r.merge(:cloud => cloud)).merge(cloud.cloud_provider.dsl_options)
        Ec2Instance.new(inst_options)
      end.first
    end

    def request_spot_instances!
      r = cloud_provider.ec2.request_spot_instances(
        :spot_price           => spot_price.to_s,
        :launch_group         => launch_group.to_s,
        :instance_count       => max_count,
        :type                 => spot_persistence,
        # TODO: valid_from, valid_until, availability_zone_group
        :image_id             => image_id,
        :key_name             => keypair.basename,
        :security_group       => cloud.security_group_names,
        :user_data            => user_data,
        :instance_type        => instance_type,
        :availability_zone    => availability_zone,
        :block_device_mapping => block_device_mapping,
        :launch_group         => launch_group,
        :base64_encoded       => true)
      p r
      return 'spot instances requested'
    end

    def terminate!
      cloud_provider.ec2.terminate_instances(:instance_id => [self.instance_id])
      cloud_provider.reset!
    end
    def self.terminate!(hsh={}); new(hsh).terminate!; end

    # list of directories and files to exclude when bundling an instance
    def rsync_excludes(array_of_abs_paths_to_exclude=nil)
      array_of_abs_paths_to_exclude ||= %w[
            /sys
            /proc
            /dev/pts
            /dev
            /media
            /mnt
            /proc
            /sys
            /etc/ssh/ssh_host_*
            /etc/ssh/moduli
            /etc/udev/rules.d/70-persistent-net.rules
            /etc/udev/rules.d/z25_persistent-net.rules
            ]
      array_of_abs_paths_to_exclude.inject(''){|str, path| str << "--exclude=#{path}" ; str}
    end

    # create an image file and copy this instance to the image file.
    def make_image(opts={})
      opts = {:volume       => '/',
              :size         => 6000,
              :destination  => '/mnt/bundle',
              :exclude      => nil
              }.merge(opts)
      image_file = File.join(opts[:destination], opts[:prefix] )
      cmds = ["mkdir -p #{opts[:destination]}"]
      cmds << "dd if=/dev/zero of=#{image_file} bs=1M count=#{opts[:size]}"
      cmds << "mkfs.ext3 -F -j #{image_file}"
      cmds << "mkdir -p #{opts[:destination]}/loop"
      cmds << "mount -o loop #{image_file} #{opts[:destination]}/loop"
      cmds << "rsync -ax #{rsync_excludes(opts[:exclude])} #{opts[:volume]}/ #{opts[:destination]}/loop/"
      cmds << "if [[ -f /etc/init.d/ec2-ssh-host-key-gen ]]; then chmod u+x /etc/init.d/ec2-ssh-host-key-gen ;fi"
      cmds << "umount #{opts[:destination]}/loop"
      self.ssh cmds
      image_file
    end

    # TODO: WIP:  bundle up the instance and register it as a new ami.
    # An image of the running node will be creatd, or
    # if a path to an image file on the remote node is given, that will be used
    def bundle_and_register(img=nil, opts={})
      opts = {:cert         => cert,
              :bucket       => nil,
              :prefix       => image_id,
              :kernel       => kernel_id,
              :ramdisk      => ramdisk_id,
              :ec2cert      => cloud_cert
              }.merge(opts)
      raise "You must specify a bucket to bundle to" if opts[:bucket].nil?
      scp ec2cert, "/mnt/bundle/"
      scp cert, "/mnt/bundle/"
      arch = self[:instanceType].match(/m1\.small|c1\.medium/) ? 'i386' : 'x86_64'
      image = img ? img : make_image(opts)
      ssh "ec2-bundle-image #{image} -d /mnt/bundle -u #{self[:ownerId]} -k /mnt/bundle/pk-*.pem -c /tmp/cert-*.pem"
      manifest = "/mnt/bundle/#{opts[:prefix]}.manifest.xml"
      ssh "ec2-upload-bundle -a #{access_key} -s #{secret_access_key} -m #{manifest}"
      ami_str = ssh "ec2-register-bundle"
      ami = ami_str.grep(/ami-\w*/).first
      return ami
    end

  end
end
