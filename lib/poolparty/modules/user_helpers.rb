module PoolParty
  # Require all the files in a directory below the base
  def require_user_directory(dirname)    
    begin
      cloud_dir = ::File.dirname($pool_specfile)
      Dir["#{cloud_dir}/#{dirname}/**"].each {|a| require a }
    rescue Exception => e
      vputs "Error requiring user directory #{dirname}: #{e}"
    end
  end

  # Add to the suitcase files below the clouds.rb base directory
  def pack_user_directory(dirname)
    begin
      ::Suitcase::Zipper.add("#{::File.dirname($pool_specfile)}/#{dirname}", "/etc/poolparty")
    rescue Exception => e
      vputs "Error packing user directory #{dirname}: #{e}"
    end    
  end
end