module CloudProviders
  class ElasticBlockStore < Ec2Helper

    # instance methods
    attr_accessor :volumeId, :size, :snapshotId, :status, :createTime, :attachments, :device, :availabilityZone

    alias :volume_id :volumeId
    alias :snapshot_id :snapshotId
    alias :availability_zone :availabilityZone 
    alias :create_time :createTime

    def initialize(raw_response,init_opts={},&block)
      parse_raw_response(raw_response)
      super(volumeId,init_opts,block)
    end

    def parse_raw_response(raw_response)
      @raw_respons = raw_response
      raw_response.each{|k,v| send k, v if respond_to?(k) }
      @attachments=raw_response.attachmentSet.item
      @attachments.each{|attch| instance_id attch.instanceId if attch.status=="attached"}
    end

    def attached?(fn_instance_id=nil)
      return false unless @status=="in-use" or @status=="attaching"
      return true if fn_instance_id.nil?
      return true if fn_instance_id == instance_id
      return false
    end

    def available?
       @status=="available"
    end

    def attach(ec2_instance,device)
      res=ec2.attach_volume :volume_id => volume_id, :instance_id => ec2_instance.instance_id, :device => device
      res.return=="true"
    end

    def delete!
      res=ec2.delete :volume_id => volume_id
      res.return == "true"
    end

    def update!
      parse_raw_response ElasticBlockStoreGroup.volumes_on_ec2 volume_id
    end

    def run
      warn "ElasticBlockStore unimplemented as of now"
    end
  
  end
end
