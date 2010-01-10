module CloudProviders
  class ElasticBlockStore < Ec2Helper

    # instance methods
    attr_accessor :volumeId, :size, :snapshotId, :status, :attachments, :device, :availabilityZone, :instance_id
    attr_reader :createTime

    alias :volume_id :volumeId
    alias :snapshot_id :snapshotId
    alias :availability_zone :availabilityZone 
    alias :create_time :createTime

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
      raw_response.each{|k,v| send k+"=", v if respond_to?(k+"=") }
      if raw_response.attachmentSet.respond_to?(:item)
        @attachments=raw_response.attachmentSet.item 
        @attachments.each{|attch| instance_id=attch.instanceId if attch.status=="attached"}
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
      ec2.attach_volume(:volume_id => volume_id, :instance_id => ec2_instance.instance_id, :device => device).return=="true"
    end

    def detach
      ec2.detach_volume(:volume_id => volume_id).return=="true"
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
