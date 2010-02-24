module CloudProviders

  class ElasticIp < Ec2Helper
    
    default_options(:instance_id => nil, :public_ip => nil)
    
    def initialize(raw_response={})
      @raw_response = raw_response
      self.instance_id = raw_response["instanceId"]
      self.public_ip = raw_response["publicIp"]
    end
    
    def assign_to(node)
      puts "-----> Assigning #{public_ip} to node: #{node.instance_id}"
    end
    
    def self.unused_elastic_ips(ec2_instance)
      @unused_elastic_ips ||= elastic_ips(ec2_instance).select {|ip| ip.instance_id.nil? }
    end
    
    def self.elastic_ips(ec2_instance)
      begin
        @elastic_ips ||= ec2_instance.ec2.describe_addresses.addressesSet.item.map do |i|
          new(i)
        end
      rescue Exception => e
        []
      end
    end
    
    def self.reset!
      @elastic_ips = @unused_elastic_ips = nil
    end
    
  end

end