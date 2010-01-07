
module CloudProviders
  class ElasticBlockStore < Ec2Helper

    default_options(:device => nil, :size => 0, :snapshot_id => nil)
    alias :snapshotId :snapshot_id
    
    def volumes(*volume_ids)
      volume_ids.each{|volume_id| @volumes << ec2.volumes(:volume_id => volume_id)}
    end
    def volumes_attached_to(instanceId)
      @volumes.select {|vol| vol.attached?(instanceId)}
    end

    # get volumes that are not attached
    def free_volumes(availability_zone)
      @volumes.select{|vol| vol.available? and vol.availability_zone == availability_zone}
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
      vol=ElasticBlockStore.new(ec2.create_volume options,:cloud => cloud)
      @volumes<<vol
      vol
    end
    
    def attach(nodes)
      nodes.each{|node|
        # Check no volumes are attached to node on device
        skip_node=false
        ec2.volumes.each{|vol| 
          if vol.attached?(node.instance_id)and vol.device == device
            warn "A volume is allready attached to device #{device} of instance #{node.instance_id}" 
            skip_node = true
          end
        }
        unless skip_node
          vol=get_free_volume(node.availability_zone)
          vol.device=device
          vol.attach(node) 
        end
      }
    end
  end
end
