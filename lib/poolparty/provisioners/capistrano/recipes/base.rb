=begin rdoc
  Base provisioner capistrano tasks
=end

# Run each of these methods inside the Capistrano:Configuration context, dynamicly adding each method as a capistrano task.
Capistrano::Configuration.instance(:must_exist).load do
  
  # namespace(:base) do
  desc "Install ruby"
  def install_ruby_from_stable_source
    # Not sure which is the appropriate step to take yet... install from source or using a package manager... 
    # We'll stick with the package manager for now
    run "#{installer_for} ruby"
    # run <<-EOR
    #   cd #{remote_storage_path}/dependencies.tar/packages/
    #   tar -zxf stable-snapshot.tar.gz
    #   cd ruby
    #   ./configure
    #   make
    #   make install
    # EOR
  end
    desc "Install rubygems"
    def install_rubygems_from_stable_source
      # run "#{installer_for} ruby rubygems"
      run <<-EOR
        cd #{remote_storage_path}/dependencies.tar/packages/
        tar -zxf rubygems
        cd rubygems-1.3.1
        ruby setup.rb
      EOR
    end
    
    desc "Setup for poolparty"
    def setup_for_poolparty
      run "mkdir -p #{Default.base_config_directory}"
      put cloud.to_properties_hash.to_json, "/etc/poolparty/clouds.json"
      upload $pool_specfile, "#{Default.base_config_directory}/clouds.rb"
    end
    
    desc "Install provisioner"
    def install_provisioner
      run "#{installer_for} #{puppet_packages}"
    end
    
    desc "Create poolparty commands"
    def create_poolparty_commands
    end
  
    desc "Fix rubygems"
    def fix_rubygems
      # echo '#{open(::File.join(template_directory, "gem")).read}' > /usr/bin/gem &&
      # cp #{remote_storage_path}/gem /usr/bin/gem
      run <<-EOR
        if gem -v; then echo "gem is working"; else cp #{remote_storage_path}/gem /usr/bin/gem; fi;
        /usr/bin/gem update --system 2>&1 > /dev/null;/usr/bin/gem update --system
        GEMPATH=`gem env gempath`
        cp  $GEMPATH/bin/* /usr/bin;
        if gem -v; then echo "gem is working"; else cp #{remote_storage_path}/gem /usr/bin/gem; fi;
        echo 'gems updated!'
      EOR
    end
    
    desc "Unpack dependency store"
    def unpack_dependencies_store
      "tar -zxf #{remote_storage_path}/dependencies.tar.gz"
    end
    
    desc "Install base gems"
    def install_base_gems
      # run(returning(Array.new) do |arr|
      #   base_gems.each do |name, url|
      #     str = url.empty? ? "#{name}" : "#{Base.remote_storage_path}/#{name}.gem"
      #     arr << "/usr/bin/gem install --ignore-dependencies --no-ri --no-rdoc #{str}; echo 'insatlled #{name}'"
      #   end
      # end.join(" && "))
      run <<-EOR
        ruby -e 'Dir["#{remote_storage_path}/gems/cache/*.gem"].each {|g| `/usr/bin/gem install --ignore-dependencies --no-ri --no-rdoc \#\{g\}` }'
      EOR
    end
    
    desc "Upgrade system"
    def upgrade_system
      str = case os
      when :ubuntu
        "
echo 'deb http://mirrors.kernel.org/ubuntu hardy main universe' >> /etc/apt/sources.list &&
aptitude update -y
        "
      else
        "echo 'No system upgrade needed'"
      end
      run str
    end
    
    # DEPRECATE
    desc "Upgrade rubygems"
    def upgrade_rubygems
      
    end
    
    desc "Make log directory"
    def make_log_directory
      run "mkdir -p /var/log/poolparty"
    end
    
    desc "Create ssl storage directories for poolparty"
    def create_poolparty_ssl_store
      run <<-EOR
        mkdir -p #{poolparty_config_directory}/ssl/private_keys &&
        mkdir -p #{poolparty_config_directory}/ssl/certs &&
        mkdir -p #{poolparty_config_directory}/ssl/public_keys
      EOR
    end
    desc "Add erlang cookie"
    def write_erlang_cookie
      # cookie = (1..16).collect { chars[rand(chars.size)] }.pack("C*")
      cookie =  (1..65).collect {rand(9)}.join()
      put( cookie, '/root/.erlang.cookie', :mode => 400 )
    end
    
    desc "Setup basic poolparty structure"
    def setup_basic_poolparty_structure
      run <<-EOR
        echo "Creating basic structure for poolparty" &&
        mkdir -p /etc/puppet/manifests/nodes  &&
        mkdir -p /etc/puppet/manifests/classes &&
        echo "import 'nodes/*.pp'" > /etc/puppet/manifests/site.pp &&
        echo "import 'classes/*.pp'" >> /etc/puppet/manifests/site.pp          
      EOR
    end
    
    desc "Setup shareable file system for provisioner"
    def setup_provisioner_filestore
      run <<-EOR
        echo '[files]' > /etc/puppet/fileserver.conf &&
        echo '  path #{remote_storage_path}' >> /etc/puppet/fileserver.conf &&
        echo '  allow *' >> /etc/puppet/fileserver.conf &&
        mkdir -p /var/poolparty/facts &&
        mkdir -p /var/poolparty/files &&
        mkdir -p #{base_config_directory}
      EOR
    end
    
    desc "Setup autosigning for provisioner"
    def setup_provisioner_autosigning
      run "echo \"*\" > /etc/puppet/autosign.conf"
    end
    
    desc "ensure gem binaries are copied to /usr/bin/"
    def copy_gem_bins_to_usr_bin
      run 'cp /usr/lib/ruby/gems/1.8/gems/*/bin/* /usr/bin'
    end

    
  # end
end