module PoolParty
  module CloudDsl
        
    def mount_ebs_volume_at(id="", loc="/data")
      ebs_volume_id id
      ebs_volume_mount_point loc
      ebs_volume_device "/dev/#{id.sanitize}"
      has_directory(:name => loc)
      has_mount(:name => loc, :device => ebs_volume_device)
    end
    
  end
end