=begin rdoc
  ssh key used to login to remote instances\
=end
class Keypair

  include SearchablePaths
  has_searchable_paths(:prepend_paths => [Dir.pwd, '/etc/poolparty/keys', "#{ENV["HOME"]}/.ssh/", "#{ENV["HOME"]}/.ec2/", ENV['EC2_CONFIG_DIR']])

  # Amazon will not append suffix, but public key may have '.pem' suffix
  SEARCH_SUFFIXES = %w( .pem )

  attr_accessor :filepath
  attr_reader :extra_paths, :opts
  attr_reader :search_suffixes

  # Create a new key that defaults to id_rsa as the name.
  def initialize(fpath, extra_paths=[], opts={})
    @filepath = fpath
    @opts = opts
    @extra_paths = [extra_paths].flatten.map {|a| File.expand_path(a) }
    @search_suffixes = SEARCH_SUFFIXES
  end

  # If the full_filepath is nil or false, then the key doesn't exist
  def exists?
    !! full_filepath
  end

  # Read the content of the key
  def content
    @content ||= exists? ? open(full_filepath).read : nil
  end

  # Returns the full_filepath of the key. If a full filepath is passed, we just return the expanded filepath
  # for the keypair, otherwise query where it is against known locations
  def full_filepath
    @full_filepath ||=
      find_file_in_path_with_suffix(filepath, extra_paths,
                                    search_suffixes) || false
  end

  def to_s
    basename
  end

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
    @basename ||= ::File.basename(filepath, ::File.extname(filepath))
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
    if exists?
      true
    else
      raise StandardError.new("#{filepath} key file cannot be found") unless filepath.nil?
    end
  end

  # try filename with suffix and without suffixes.
  # Checks all paths without suffix first, then try all paths for all suffixes.
  def find_file_in_path_with_suffix(file, extra_paths, suffixes=[],
                                    try_wo_suffix=true)
    suffixes_to_try = suffixes.dup
    suffixes_to_try.push '' if try_wo_suffix

    suffixes_to_try.map {|s| file + s }.each do |suffixed|
      fullpath = if File.file?(File.expand_path(suffixed))
                   ::File.expand_path(suffixed)
                 else
                   search_in_known_locations(suffixed, extra_paths)
                 end
      return fullpath if fullpath
    end

    nil
  end

end
