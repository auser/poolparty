Capistrano::Configuration.instance(:must_exist).load do

  desc "FIXME: temporary, ec2 specific hack untill server binaries are refactored"
  def copy_ec2_poolparty_server_binaries
    upload( "#{::File.dirname(__FILE__)}/../../../../../bin/ec2-list-active", '/usr/bin/server-list-active', :mode=>755)
    # Dir["#{::File.dirname(__FILE__)}/../../../../../bin/*"].each{ |f|
    #   put f, '/usr'}      
  end
  
  desc "Put poolparty keys"
  def put_poolparty_keys
    put keypair.full_filepath, remote_keypair_path, :mode => 600
  end
  
  desc "place the aws keys on the server at /etc/poolparty/aws_keys"
  def put_aws_credintials_on_server
    aws_keys={:access_key => access_key, :secret_access_key => secret_access_key}
    put aws_keys.to_yaml, '/etc/poolparty/aws_keys.yml', :mode => 400
  end
  
end