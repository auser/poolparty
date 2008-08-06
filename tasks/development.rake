namespace(:dev) do
  task :initialize do
    setup_application
    run "mkdir ~/.ec2 >/dev/null 2>/dev/null" unless File.directory?("~/.ec2")      
  end
  # Setup a basic development environment for the user 
  desc "Setup development environment specify the config_file"
  task :setup => [:initialize, :setup_keypair] do    
    certloc = "#{Application.ec2_dir}/#{Application.keypair}/cert-*.pem 2>/dev/null"
    pkloc = "#{Application.ec2_dir}/#{Application.keypair}/pk-*.pem 2>/dev/null"
    unless `ls #{certloc}`.length > 1 && `ls #{pkloc}`.length > 1
      puts <<-EOM
Make sure you run rake dev:setup_pemkeys before you run this command

I cannot continue until your keys are setup. 
exiting...
      EOM
      exit
    end
    keyfilename = ".#{Application.keypair}_pool_keys"
    run <<-EOR
      echo 'export AWS_ACCESS_KEY=\"#{Application.access_key}\"' > $HOME/#{keyfilename}
      echo 'export AWS_SECRET_ACCESS=\"#{Application.secret_access_key}\"' >> $HOME/#{keyfilename}
      echo 'export EC2_HOME=\"#{Application.ec2_dir}\"' >> $HOME/#{keyfilename}
      echo 'export KEYPAIR_NAME=\"#{Application.keypair}\"' >> $HOME/#{keyfilename}
      echo 'export EC2_PRIVATE_KEY=`ls ~/.ec2/#{Application.keypair}/pk-*.pem`;' >> $HOME/#{keyfilename}
      echo 'export EC2_CERT=`ls ~/.ec2/#{Application.keypair}/cert-*.pem`;' >> $HOME/#{keyfilename}
    EOR
    puts <<-EOM
To work on this cloud, source the file like: 
  
  source $HOME/#{keyfilename}
  
    EOM
  end
  desc "Generate a new keypair"
  task :setup_keypair => [:initialize] do
    Application.keypair ||= "#{File.basename(Dir.pwd)}"    
    run "ec2-delete-keypair #{Application.keypair}" if File.file?(Application.keypair_path)
    puts "-- setting up keypair named #{Application.keypair} in #{Application.keypair_path}"
    run <<-EOR        
      chmod 600 #{Application.keypair_path} 2>/dev/null
      mkdir ~/.ec2/#{Application.keypair} 2>/dev/null
      ec2-add-keypair #{Application.keypair} > #{Application.keypair_path}
    EOR
  end
  desc "Setup pem keys"
  task :setup_pemkeys => [:initialize] do    
    puts "Setting up stubbed pem keys in ~/.ec2/#{Application.keypair}"
    run <<-EOR
      mkdir -p ~/.ec2/#{Application.keypair} 2>/dev/null
      echo 'UPDATE ME' > #{Application.ec2_dir}/#{Application.keypair}/cert-UPDATEME.pem
      echo 'UPDATE ME' > #{Application.ec2_dir}/#{Application.keypair}/pk-UPDATEME.pem
    EOR
    puts "Don't forget to replace your ~/.ec2/#{Application.keypair}/*.pem keys with the real amazon keys"
  end
  desc "initialize setup"
  task :init => [:setup_pemkeys]
  
  desc "Just an argv test"
  task :test => :initialize do
    puts "---- Testing ----"
    puts PoolParty.options(ARGV.dup)
    puts "Using keypair at: #{Application.keypair_path}"
  end
  desc "Installation listing"
  task :list_install => :initialize do
    puts "-- packages to install --"
    Provider.install_PoolParty(true)
  end
  desc "Authorize base ports for application"
  task :authorize_ports => :initialize do
    run <<-EOR
      ec2-authorize -p 22 default
      ec2-authorize -p 80 default
    EOR
  end
end