namespace :poolparty do
  namespace(:ec2) do
    
    task :initialize do
      Kernel.system"mkdir ~/.ec2 >/dev/null 2>/dev/null" unless File.directory?("~/.ec2")      
    end
    # Setup a basic development environment for the user 
    
    desc "Setup development environment specify the config_file"
    task :setup => [:initialize, :setup_keypair] do    
      certloc = "#{Default.ec2_dir}/#{Default.keypair}/cert-*.pem 2>/dev/null"
      pkloc = "#{Default.ec2_dir}/#{Default.keypair}/pk-*.pem 2>/dev/null"
      unless `ls #{certloc}`.length > 1 && `ls #{pkloc}`.length > 1
        puts <<-EOM
  Make sure you run rake dev:setup_pemkeys before you run this command

  I cannot continue until your keys are setup. 
  exiting...
        EOM
        exit
      end
      keyfilename = ".#{Default.keypair}_pool_keys"
      run <<-EOR
        echo 'export AWS_ACCESS_KEY=\"#{Default.access_key}\"' > $HOME/#{keyfilename}
        echo 'export AWS_SECRET_ACCESS=\"#{Default.secret_access_key}\"' >> $HOME/#{keyfilename}
        echo 'export EC2_HOME=\"#{Default.ec2_dir}\"' >> $HOME/#{keyfilename}
        echo 'export KEYPAIR_NAME=\"#{Default.keypair}\"' >> $HOME/#{keyfilename}
        echo 'export EC2_PRIVATE_KEY=`ls ~/.ec2/#{Default.keypair}/pk-*.pem`;' >> $HOME/#{keyfilename}
        echo 'export EC2_CERT=`ls ~/.ec2/#{Default.keypair}/cert-*.pem`;' >> $HOME/#{keyfilename}
      EOR
      puts <<-EOM
  To work on this cloud, source the file like: 
  
    source $HOME/#{keyfilename}
  
      EOM
    end
    
    desc "Generate a new keypair"
    task :setup_keypair => [:initialize] do
      Default.keypair ||= "#{File.basename(Dir.pwd)}"    
      run "ec2-delete-keypair #{Default.keypair}" if File.file?(Default.keypair_path)
      puts "-- setting up keypair named #{Default.keypair} in #{Default.keypair_path}"
      run <<-EOR        
        chmod 600 #{Default.keypair_path} 2>/dev/null
        mkdir ~/.ec2/#{Default.keypair} 2>/dev/null
        ec2-add-keypair #{Default.keypair} > #{Default.keypair_path}
      EOR
    end
    
    desc "Setup pem keys"
    task :setup_pemkeys => [:initialize] do    
      puts "Setting up stubbed pem keys in ~/.ec2/#{Default.keypair}"
      run <<-EOR
        mkdir -p ~/.ec2/#{Default.keypair} 2>/dev/null
        echo 'UPDATE ME' > #{Default.ec2_dir}/#{Default.keypair}/cert-UPDATEME.pem
        echo 'UPDATE ME' > #{Default.ec2_dir}/#{Default.keypair}/pk-UPDATEME.pem
      EOR
      puts "Don't forget to replace your ~/.ec2/#{Default.keypair}/*.pem keys with the real amazon keys"
    end
    
    desc "initialize setup"
    task :init => [:setup_pemkeys]
    desc "Authorize base ports for application"
    task :authorize_ports => :initialize do
      run <<-EOR
        ec2-authorize -p 22 default
        ec2-authorize -p 80 default
      EOR
    end
    
    desc "Turn the gemspec into a yaml file"
    task :gemspec_to_yaml => [:initialize, :gemspec] do
      filepath = ::File.join(::File.dirname(__FILE__), "..", "poolparty.gemspec")
      data = open(filepath).read
      spec = eval("$SAFE = 3\n#{data}")
      yml = YAML.dump spec
      File.open(filepath, "w+"){ |f|f << yml }
    end
    
  end
end

namespace :ctags do
  desc "create ctags"
  task :create do
    dir = File.dirname(__FILE__) + "/.."
    Dir.chdir(dir)
    sh "time ctags -R --language-force=Ruby -F ./TAGS ."
  end
end