module CloudProviders
    class ElasticBlockStore < Ec2Helper
  
      default_options(:vol_id => nil, :availability_zone => nil, :instance_id => nil, :device_on_instance => nil)
      
      #Class methods
      def self.volumes(filters=nil)
        @all_volumes=ec2.describe_volumes.volumeSet.item unless @all_volumes
          @all_volumes.map {|vol|
            new vol if filters.nil? or vol.values_at(*filters.keys.map{|key| key.to_s})==filters.values
          }.compact
      end
      def self.volumes_attached_to(instanceId)
        volumes.select {|vol| attached? }
      end

      def self.create(options)
        options.values.map!{|v| v.to_s}
        new ec2.create_volume options
      def

      # instance methods
      attr_accessor :volumeId, :size, :snapshotId, :status, :availabilityZone, :createTime, :attachments
      alias volume_id volumeId
      alias snapshot_id snapshotId
      alias availability_zone availabilityZone
      alias create_time createTime

      def initialize(raw_response)
        parse_raw_response(raw_response)
      end

      def parse_raw_response(raw_response)
        @raw_respons = raw_response
        raw_response.each{|k,v| send k, v if respond_to?(k) }
        @attachemnts=raw_response.attachmentSet.item
      end

      def attached?
        @status=="in-use"
      def

      def attach(ec2_instance,device)
        res=ec2.attach_volume :volume_id => volume_id, :instance_id => ec2_instance.instance_id, :device => device
        res.return=="true"
      end

      def delete!
        res=ec2.delete :volume_id => volume_id
        res.return == "true"
      end

      def update!
        parse_raw_response volumes volume_id
      end

    def run
      warn "ElasticBlockStore unimplemented as of now"
    end
  
  end
end
