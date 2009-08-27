=begin rdoc
=end

module PoolParty
  module Resources
    
    class Hermes < Resource
      
      default_options(
        :name => nil
      )
      
      # def after_loaded
        # has_exec(:name => "git-#{name}", :creates => creates_dir ) do
        #   # Cloud, GitRepos, Exec
        #   if requires_user
        #     command("git clone #{requires_user}@#{repository} #{dir}")
        #   else
        #     command("cd #{dir} && git clone #{repository}")
        #   end
        #
        #   cwd "#{dir if dir}"          
        #   requires get_directory("#{dir}")
        # end
      # end

      def after_loaded
        add_unpack
        run_if_needed
      end

      def after_compile
        run_dependencies
        build_rsync_directory
      end

      def run_dependencies
        case cloud.platform
        when false
        else
          has_package "erlang-nox"
          has_package "erlang-dev"
          has_package "rrdtool"
        end
      end

      def build_rsync_directory
        p cloud.tmp_path
        hermes_dir = cloud.tmp_path + "/tmp/hermes"
        FileUtils.mkdir_p(hermes_dir)
        FileUtils.cp(hermes_release_tar_gz, hermes_dir)
        FileUtils.cp(target_system_file, hermes_dir)
      end

      def add_unpack
        has_exec "cd /tmp/hermes && escript target_system install hermes-#{hermes_release_version} #{remote_hermes_deployed_dir}", 
          :creates => "#{remote_hermes_deployed_dir}/releases/#{hermes_release_version}"
      end

      def run_if_needed
        has_exec "#{remote_hermes_deployed_dir}/bin/erl -boot #{remote_hermes_deployed_dir}/releases/#{hermes_release_version}/start -noshell -detached", 
          :not_if => "ps aux | grep -v grep | grep hermes | grep beam"
      end

      private

      def hermes_release_tar_gz
        `#{hermes_dir}/scripts/most_recent_release tar.gz`.strip
      end

      def target_system_file
        "#{hermes_dir}/scripts/target_system"
      end

      def hermes_dir 
        PoolParty.lib_dir + "/vendor/erlang/hermes"
      end

      def remote_hermes_deployed_dir
        "/var/poolparty/hermes"
      end

      def hermes_release_version
        File.basename(hermes_release_tar_gz).gsub(/hermes-(.*?)\.tar\.gz/, '\1')
      end
      
    end
  end
end

# == install
# upload tar.gz & target_system
# run target_system unless VERSION exists
# run erlang: /usr/local/erl-target/bin/erl -boot /usr/local/erl-target/releases/FIRST/start _unless_ already running
