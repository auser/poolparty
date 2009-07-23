=begin rdoc
  ssh key used to login to remote instances\
=end
class Keypair
  
  include SearchablePaths
  has_searchable_paths(:paths => ["#{ENV["HOME"]}/.ssh/", "#{ENV["HOME"]}/.ec2/"])
  
  attr_accessor :filepath
  
  # Create a new key that defaults to id_rsa as the name. 
  def initialize(fpath=nil)
    @filepath = fpath
    valid?
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
    @full_filepath ||= if File.file?(::File.expand_path(filepath))
      ::File.expand_path(filepath)
      else
        search_in_known_locations(filepath)
      end
  end
  alias :to_s :full_filepath
  
  #TODO: gracefully handle the case when a passpharase is needed
  # Generate a public key from the private key
  # net/ssh already has this built-in from our extension.
  def public_key
    if !@public_key_string || @public_key_string.empty?
      pkey = Net::SSH::KeyFactory.load_private_key(full_filepath)
      @public_key_string = pkey.public_key
    else
      @public_key_string
    end
  end
  
  def public_key=(str)
     @public_key_string = str
  end
  
  # Basename of the keypair
  def basename
    @basename ||= ::File.basename(full_filepath, ::File.extname(full_filepath))
  end
  
  # Just the filename of the keypair
  def filename
    @filename ||= ::File.basename(full_filepath) rescue filepath
  end
  
  # Support to add the enumerable each to keys
  def each
    yield full_filepath
  end
      
  # Validation checks
  # if all of the validations pass, the object is considered valid
  # the validations are responsible for raising a PoolPartyError (StandardError)
  def valid?
    validations.each {|validation| self.send(validation.to_sym) }
  end

  private

  # Validations
  def validations
    [:keypair_found?, :has_proper_permissions?]
  end
  
  # Check the proper permissions
  def has_proper_permissions?
    perm_truth = [:readable?, :writable?, :executable?].map {|meth| File.send(meth, full_filepath)} == [true, true, false]
    raise StandardError.new("Your keypair #{full_filepath} has improper file permissions. Keypairs must be 0600 permission. Please chmod your keypair file and try again") unless perm_truth
  end
  
  def keypair_found?
    raise StandardError.new("#{filepath} key file cannot be found") unless full_filepath
  end
  
end