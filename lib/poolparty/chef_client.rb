module PoolParty
  class ChefClient < Chef
    dsl_methods :server_url,:validation_token
    
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
      return @_roles||=cloud.name if roles.empty?
      @_roles=roles
    end

    def compile!
      build_tmp_dir
    end

    private
    def chef_cmd
      return <<-CMD
        $GEM_BIN/chef-client -j /etc/chef/dna.json -c /etc/chef/client.rb"
      CMD
    end
    # The NEW actual chef resolver.
    def build_tmp_dir
      base_directory = tmp_path/"etc"/"chef"
      FileUtils.rm_rf base_directory
      FileUtils.mkdir_p base_directory   
      puts "Creating the dna.json"
      attributes.to_dna [], base_directory/"dna.json", {:run_list => roles.map{|r| "role[#{r}]"} + @_recipes.map{|r| "recipe[#{r}]"}}
      write_client_dot_rb
    end
    
    def write_client_dot_rb(to=tmp_path/"etc"/"chef"/"client.rb")
      content = <<-EOE
log_level          :info
log_location       "/var/log/chef/client.log"
ssl_verify_mode    :verify_none
file_cache_path    "/var/cache/chef"
pid_file           "/var/run/chef/client.pid"
Chef::Log::Formatter.show_time = true
openid_url         #{openid_url}
      EOE
      %w(search_url role_url remotefile_url template_url registration_url).each{|url|
        content+="#{url}   #{server_url}\n"
      }
      content+="validation_token  #{validation_token}\n" if validation_token
      File.open(to, "w") do |f|
        f << content
      end
    end
  end
end
