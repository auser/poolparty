=begin rdoc
  ssh key used to login to remote instances
=end
module PoolParty
  class Key
    include SearchablePaths
    has_searchable_paths(:dirs => ["/", "keys"], :prepend_paths => ["#{ENV["HOME"]}/.ssh"])
    
    attr_accessor :filepath
    
    # Create a new key that defaults to id_rsa as the name. 
    def initialize(fpath=nil)
      @filepath = (fpath.nil? || fpath.empty?) ? "id_rsa" : fpath
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
      @full_filepath ||= ::File.file?(::File.expand_path(filepath)) ? ::File.expand_path(filepath) : search_in_known_locations(filepath)
    end
    alias :to_s :full_filepath
    
    #TODO: gracefully handle the case when a passpharase is needed
    # Generate a public key from the private key
    def public_key
      if !@public_key_string || @public_key_string.empty?
         @public_key_string = `ssh-keygen -y -f #{full_filepath}`
         raise 'Unable to generate public_key_string' if @public_key_string.empty?
      else
        @public_key_string
      end
    end
    
    def public_key=(str)
       @public_key_string = str
    end
    
    # Basename of the keypair
    def basename
      @basename ||= ::File.basename(full_filepath, ::File.extname(full_filepath)) rescue filepath
    end
    
    # Just the filename of the keypair
    def filename
      @filename ||= ::File.basename(full_filepath) rescue filepath
    end
    
    # Support to add the enumerable each to keys
    def each
      yield full_filepath
    end
    
    # Turn the keypair into the a useful json string
    def to_json
      "{\"basename\":\"#{basename}\",\"full_filepath\": \"/etc/poolparty/#{filename}\"}"
    end
    
  end
end