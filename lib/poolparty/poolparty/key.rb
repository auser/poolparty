# STUB FOR NOW
# TODO
module PoolParty
  class Key
    
    attr_accessor :filepath
    
    # Create a new key that defaults to id_rsa as the name. 
    def initialize(filepath=nil)
      @filepath = (filepath.nil? || filepath.empty?) ? "id_rsa" : filepath
    end
    
    # If the full_filepath is nil, then the key doesn't exist
    def exists?
      full_filepath != nil
    end
    
    # Read the content of the key
    def content
      @content ||= exists? ? open(full_filepath).read : nil
    end
        
    # Returns the full_filepath of the key. If a full filepath is passed, we just return the expanded filepath
    # for the keypair, otherwise query where it is against known locations
    def full_filepath
      @full_filepath ||= ::File.file?(::File.expand_path(filepath)) ? ::File.expand_path(filepath) : search_in_known_locations
    end
    alias :to_s :full_filepath
    
    # Basename of the keypair
    def basename
      @basename ||= ::File.basename(full_filepath, ::File.extname(full_filepath)) rescue filepath
    end
    
    # Just the filename of the keypair
    def filename
      @filename ||= ::File.basename(full_filepath) rescue filepath
    end
    
    # Search for the key in default locations with the entire filepath
    # if the file exists. If it doesn't exist in the default locations, 
    # then it returns nil and assumes we it doesn't exist
    def search_in_known_locations
      self.class.keypair_paths.each do |path|
        full_path = ::File.join( ::File.expand_path(path), filepath)
        return full_path if ::File.exists?(full_path)
      end
      nil
    end
    
    # Default locations to search for the key
    def self.keypair_paths
      [ "#{ENV["HOME"]}/.ssh",
        "#{Default.poolparty_home_path}/keys",
        Default.base_keypair_path,
        Default.base_config_directory,
        Default.base_ssh_path,
        Default.remote_storage_path,        
        Dir.pwd
      ]
    end
    
    # Support to add the enumerable each to keys
    def each
      yield full_filepath
    end
    
    # Turn the keypair into the a useful json string
    def to_json
      "{\"basename\":\"#{basename}\",
       \"full_filepath\": \"/etc/poolparty/#{filename}\"}"
    end
    
  end
end