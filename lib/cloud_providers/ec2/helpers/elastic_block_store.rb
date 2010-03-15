module CloudProviders
  class ElasticBlockStore < Ec2Helper

    # instance methods
    attr_reader :volumeId, :size, :snapshotId, :status, :attachments, :device, :availabilityZone, :instanceId
    attr_reader :createTime 

    alias :volume_id :volumeId
    alias :snapshot_id :snapshotId
    alias :availability_zone :availabilityZone 
    alias :create_time :createTime
    alias :instance_id :instanceId

    def createTime(create_time)
      unless create_time.class==DateTime
        @create_time=(DateTime.new(create_time) rescue nil) 
      else
        @createTime=create_time
      end
    end
    def initialize(raw_response,init_opts={},&block)
      parse_raw_response(raw_response)
      super(volumeId,init_opts,&block)
    end

    def parse_raw_response(raw_response)
      @raw_respons = raw_response
      raw_response.each{|k,v| instance_variable_set("@"+k,v) if respond_to?(k) }
      unless raw_response.attachmentSet.nil?
        @attachments=raw_response.attachmentSet.item 
        @attachments.each{|attch| if attch.status=="attached" or attch.status=="attaching"
            @instanceId=attch.instanceId 
            @device=attch.device
          end
        }
      end
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
      if ec2.attach_volume(:volume_id => volume_id, :instance_id => ec2_instance.instance_id, :device => device).return=="true"
        update!
        return true
      end
      false
    end

    def detach
      if ec2.detach_volume(:volume_id => volume_id).return=="true"
        update!
        return true
      end
      false
    end

    def detach!
      ec2.detach_volume(:volume_id => volume_id, :force => true).return=="true"
    end

    def delete!
      ec2.delete(:volume_id => volume_id).return == "true"
    end

    def update!
      parse_raw_response ec2.describe_volumes(:volume_id => volume_id)
    end
  
  end
end
