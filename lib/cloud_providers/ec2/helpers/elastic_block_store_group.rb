
module CloudProviders
  # ElasticBlockStoreGroup class allows easy manipulation of EBS volumes matching defined criterias.
  # Existing volumes in *cloud*'s availability zones that match the criterias will be selected for the group. When cloud instances need to attach EBS volumes from the group, the attach method should called.
  # When attaching volumes the ElasticBlockStoreGroup will select existing (unattached) volumes until there are non, afterwhich the group will create new volumes according to the criterias given as needed.
  #
  # Currently, EBS volumes will not be deleted when tearing down a cloud. This is because poolparty is stateless and thus deleting drives from it will probably result in catastroph (deletions will be too general and delete stuff you don't want deleted).
  # Hopefully, we will come up with a scheme for a deletion flag of some sort to solve this situation.
  class ElasticBlockStoreGroup < Ec2Helper

    default_options(:device => nil, :size => 0, :snapshot_id => nil)
    alias :snapshotId :snapshot_id
    
    def initialize(name=cloud.proper_name, init_opts={}, &block)
      @volumes=[]
      super
    end
    def after_initialized
      unless @volumes.size > 0 
        filters={:size => size, :availabilityZone => availability_zones}
        filters[:snapshotId]=snapshot_id if snapshot_id
        @volumes=cloud.list_ec2_volumes filters
      end
    end
    def volumes(*volume_ids)
      return @volumes if volume_ids.size==0
      volume_ids.each{|volume_id| @volumes << cloud.list_ec2_volumes(:volumeId => volume_id)}
    end
    def volumes_attached_to(instanceId)
      @volumes.select {|vol| vol.attached?(instanceId)}
    end

    # get volumes that are not attached
    def free_volumes(availability_zone)
      @volumes.flatten.select{|vol| vol.available? and vol.availability_zone == availability_zone}
    end
    # Get a free volume from existing volumes in group or create a new one
    def get_free_volume(availability_zone)
      free=free_volumes(availability_zone)
      if free.size>=1
        return free[0]
      end
      create(availability_zone)
    end

    # Create new volume on availability_zone
    def create(availability_zone)
      options={:availability_zone => availability_zone, :size => size.to_s}
      options[:snapshot_id]=snapshot_id if snapshot_id
      vol=ElasticBlockStore.new(ec2.create_volume(options),:cloud => cloud)
      @volumes<<vol
      vol
    end
    
    def attach(nodes)
      nodes.each{|node|
        # Check no volumes are attached to node on device
        skip_node=false
        cloud.list_ec2_volumes.each{|vol| 
          if vol.attached?(node.instance_id) and vol.device == device
            warn "A volume is allready attached to device #{device} of instance #{node.instance_id}" 
            skip_node = true
          end
        }
        unless skip_node
          vol=get_free_volume(node.zone)
          vol.attach(node,device) 
        end
      }
    end
    
    def verify_attachments(nodes)
      nodes_without_volume=nodes.select do |node|
        volumes_attached_to(node.id).size=0
      end
      attach nodes_without_volume if nodes_without_volume.any?
    end
  end
end
