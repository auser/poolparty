require 'test_helper'

class KeypairTest < Test::Unit::TestCase
  context "Base" do
    setup do
      Keypair.searchable_paths << File.join(FIXTURES_PATH, "keys")
      @keypair = Keypair.new(File.join(FIXTURES_PATH, "keys", "test_key"))
      @keypair_pem = Keypair.new(File.join(FIXTURES_PATH, "keys", "pem_key"))
    end
    
    should "set the file given as the file for the keypair" do
      assert_equal @keypair.filepath, File.join(FIXTURES_PATH, "keys", "test_key")
      assert_equal @keypair.full_filepath, File.expand_path(File.join(FIXTURES_PATH, "keys", "test_key"))
      assert_match @keypair.to_s, File.expand_path(File.join(FIXTURES_PATH, "keys", "test_key"))
    end

    should "find the suffixed file given without pem suffix" do
      assert_equal @keypair_pem.filepath, File.join(FIXTURES_PATH, "keys", "pem_key")
      assert_equal @keypair_pem.full_filepath, File.expand_path(File.join(FIXTURES_PATH, "keys", "pem_key.pem"))
      assert_match @keypair_pem.to_s, File.expand_path(File.join(FIXTURES_PATH, "keys", "pem_key"))
    end

    should "find the suffixed file given without pem suffix or dir" do
      basename = File.basename(@keypair_pem.full_filepath)
      basename_no_suffix = File.basename(basename, '.pem')

      search_dirs = [File.dirname(@keypair_pem.full_filepath)]

      keypair = Keypair.new(basename, search_dirs)
      keypair_no_suffix = Keypair.new(basename_no_suffix, search_dirs)

      assert_equal keypair.full_filepath, @keypair_pem.full_filepath
      assert_equal keypair_no_suffix.full_filepath, @keypair_pem.full_filepath
    end
    
    should "have the content of the file available" do
      assert_equal @keypair.content, open(File.join(FIXTURES_PATH, "keys", "test_key")).read
    end
    
    should "be able to generate the public key from the private" # do
    #       # assert_equal @keypair.public_key, "#{open(FIXTURES_PATH/"test_pub_key").read}"
    #     end
    
    should "have the basename of the keypair" do
      assert_equal @keypair.basename, "test_key"
      assert_equal @keypair.filename, "test_key"      
    end
    
    should "be valid if it's 600 permissions" do
      assert @keypair.valid?
    end
    
    should "be invalid if the file permissions are executable" do
      assert_raises StandardError do
        Keypair.new(File.join(FIXTURES_PATH, "bad_perms_test_key")).valid?
      end
    end
  end  
end
