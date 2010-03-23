require 'uri' # for URI.parse in write_bootstrap_files

module PoolParty
  # Chef class bootstrapping chef-client.
  class ChefClient < Chef
    dsl_methods :server_url,:validation_token, :validation_key, :validation_client_name

    # When init_style.nil?, old behavior is used (just run the client).
    # If init_style is specified, bootstrap::client cookbook is executed
    # To this init style.
    dsl_methods :init_style

    def openid_url(url=nil)
      if url.nil?
        return @openid_url||= (u=URI.parse(server_url)
        u.port=4001
        openid_url u.to_s)
      else
        @openid_url=url 
      end
    end
    
    def roles(*roles)
      return @_roles||=[cloud.name] if roles.empty?
      @_roles=roles
    end

    private
    def after_initialized
      raise PoolPartyError.create("ChefArgumentMissing", "server_url must be specified!") unless server_url
    end
    def chef_bin
      "chef-client"
    end

    def chef_cmd
      # without init_style, let parent class start chef-client
      return super unless init_style

      'invoke-rc.d chef-client start'
    end

    def node_configure!(remote_instance)
      super
      cmds = 
        [ 'PATH="$PATH:$GEM_BIN" chef-solo -j /tmp/chef/chef.json -c /tmp/chef/solo.rb',
          'invoke-rc.d chef-client stop',
          'PATH="$PATH:$GEM_BIN" chef-client -j /etc/chef/dna.json -c /etc/chef/client.rb',
        ]

      remote_instance.ssh cmds
    end

    # The NEW actual chef resolver.
    def build_tmp_dir
      base_directory = tmp_path/"etc"/"chef"
      FileUtils.rm_rf base_directory
      FileUtils.mkdir_p base_directory   
      FileUtils.cp validation_key, base_directory if validation_key
      puts "Creating the dna.json"
      attributes.to_dna [], base_directory/"dna.json", {:run_list => roles.map{|r| "role[#{r}]"} + _recipes.map{|r| "recipe[#{r}]"}}.merge(attributes.init_opts)
      unless init_style then    # original style init
        write_client_dot_rb
      else
        bootstrap_tmp_dir = tmp_path/"tmp/chef"
        FileUtils.rm_rf bootstrap_tmp_dir
        FileUtils.mkdir_p bootstrap_tmp_dir
        write_bootstrap_files bootstrap_tmp_dir/"solo.rb", bootstrap_tmp_dir/"chef.json"
      end
    end
    
    def write_client_dot_rb(to=tmp_path/"etc"/"chef"/"client.rb")
      content = <<-EOE
log_level          :info
log_location       "/var/log/chef/client.log"
ssl_verify_mode    :verify_none
file_cache_path    "/var/cache/chef"
pid_file           "/var/run/chef/client.pid"
Chef::Log::Formatter.show_time = true
openid_url         "#{openid_url}"
      EOE
      %w(chef_server_url).each{|url|
        content+="#{url}   \"#{server_url}\"\n"
      }
      content+="validation_token  \"#{validation_token}\"\n" if validation_token
      content+="validation_key    \"/etc/chef/#{File.basename validation_key}\"\n" if validation_key
      content+="validation_client_name  \"#{validation_client_name}\"\n" if validation_client_name
      File.open(to, "w") do |f|
        f << content
      end
    end

    def write_bootstrap_files(solo_rb, chef_json)
      uri=URI.parse(server_url)
      # this maybe reduntant, URL should have a port in there
      uri.port=4000 if uri.port == 80 # default port for chef

      contents_solo_rb = <<-EOE
file_cache_path "/tmp/chef-solo"
cookbook_path "/tmp/chef-solo/cookbooks"
recipe_url "http://s3.amazonaws.com/chef-solo/bootstrap-latest.tar.gz"
      EOE
      File.open(solo_rb, "w") do |f| f << contents_solo_rb end

      bootstrap_json = 
        {
        :bootstrap => {
          :chef => {
            :url_type =>  uri.scheme,
            :init_style => init_style,
            :path => "/srv/chef",
            :serve_path => "/srv/chef",
            :server_fqdn => uri.host + uri.path,
            :server_port => uri.port,
          },
        },
        :run_list => [ 'recipe[bootstrap::client]' ],
      }
      if validation_client_name
        bootstrap_json[:bootstrap][:chef][:validation_client_name] = validation_client_name
      end
      ChefAttribute.new(bootstrap_json).to_dna([], chef_json)
    end
  end
end
